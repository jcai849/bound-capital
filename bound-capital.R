#!/usr/bin/env Rscript

VERSION <- "1.0.0"
DOC <- paste0("bound-capital - Determining Bound Capital through Prior Credit Pivot [version ", VERSION, "]

Usage: bound-capital [FILE] -a CAPITAL [-d DATE]
       bound-capital [FILE] [-a CAPITAL] [-d DATE] -x
       bound-capital -h | -v

Determine the total bound capital at next credit (today inclusive),
based on accumulated capital from prior credit.

Arguments:
	FILE  yaml file containing credit and debit info.
	      If '-' or missing, stdin is expected.
	-a CAPITAL --accumulated CAPITAL  Total accumulated capital.
	-d DATE --date DATE  Current Date. [default: Today]

Options:
	-h --help     Show this screen.
	-v --version  Show version.
	-x --expand   Output the yaml input with expanded dates.
")

library(docopt)
library(yaml)

main <- function(argv=commandArgs(TRUE)) {
	opts <- docopt(DOC, args=argv, version=paste0(VERSION, '\n'))
	current_date <- if (identical("Today", opts$date))
	                       Sys.Date() else as.Date(opts$date)
	ledger <- read_input(opts$FILE) |> Ledger(current_date)
	if (opts$expand) { cat(str(ledger)); quit("no") }
	accumulated_capital <- as.numeric(opts$accumulated)
	prepare(ledger, current_date) |>
	bound_capital(accumulated_capital) |>
	format(display_date=FALSE) |> cat(sep='\n')
}

prepare <- function(ledger, current_date) {
	remove_historical(ledger, current_date) |>
	remove_successive_debits() |>
	normalise()
}
remove_successive_debits <- function(ledger) {
	lapply(ledger$debits,
	       \(debit) Debits(debit$dates[1], debit$amount[1])) |>
	Ledger(ledger$credits, debits=_)
}

normalise <- function(ledger) {
	credits_collapsed <- lapply(ledger$credits, "[[", "dates") |>
	do.call(c, args=_) |>
	unique()
	# What is the earliest credit that each debit immediately succeeds?
	debit_normalised_dates <- lapply(ledger$debits, \(debit) {
		diff(debit$dates >= credits_collapsed) |> which.min()
	})
	debits <- Map(Debits, debit_normalised_dates,
	              lapply(ledger$debits, "[[", "amount"))
	credit_normalised_dates <- seq(do.call(max, debit_normalised_dates))
	credits <- list(Credits(credit_normalised_dates))
	Ledger(credits, debits)
}

remove_historical <- function(x, current_date) UseMethod("remove_historical", x)
remove_historical.Ledger <- function(x, current_date) {
	future_credits <- lapply(x$credits, remove_historical, current_date)
	next_credit <- lapply(future_credits, "[[", "dates") |>
	               do.call(min, args=_)
	future_debits <- lapply(x$debits, remove_historical,
	                        next_credit)
	Ledger(future_credits, debits=future_debits)
}
remove_historical.Credits <- function(x, current_date) {
	Credits(x$dates[x$dates >= current_date])
}
remove_historical.Debits <- function(x, current_date) {
	keep_i <- x$dates >= current_date
	Debits(x$dates[keep_i], x$amount[keep_i])
}

read_input <- function(filepath) {
	ifelse(is.null(filepath), "stdin", filepath) |>
	readLines() |> paste(collapse='\n')
}
Ledger <- function(x, ...) UseMethod("Ledger", x)
Ledger.character <- function(x, current_date) {
	yaml.load(x, handlers=list(credit=CreditSpec)) |>
	LedgerSpec() |>
	Ledger(current_date)
}
LedgerSpec <- function(x) {
	credits_i <- sapply(x, inherits, "CreditSpec")
	credits <- x[credits_i]
	debits <- x[!credits_i]
	list(credits=credits,
	     debits=lapply(debits, DebitSpec)) |>
	structure(class="LedgerSpec")

}
EntrySpec <- function(x) {
	list(date=parse_date(x), amount=x$amount) |>
	structure(class="EntrySpec")
}
CreditSpec <- function(x) {
	credit <- EntrySpec(x)
	structure(credit, class=c("CreditSpec", class(credit)))
}
DebitSpec <- function(x) {
	debit <- EntrySpec(x)
	structure(debit, class=c("DebitSpec", class(debit)))
}
parse_date <- function(x) {
	if (exists("date", x)) {
		as.Date(x$date)
	} else if (exists("from", x)) {
		c(list(from=as.Date(x$from)),
		  list(by=x$by),
		  if (exists("to", x)) list(to=as.Date(x$to)) else NULL) |>
		structure(class="UnexpandedDate")
	}
}

Ledger.LedgerSpec <- function(x, current_date) {
	lapply(x$credits, extend_dates, beyond=current_date) |>
	do.call(max, args=_) |>
	lapply(x$debits, extend_dates, beyond=_) -> debit_dates
	credit_dates <- lapply(x$credits, extend_dates,
	                       beyond=do.call(max, args=debit_dates))
	credits <- Map(Credits, credit_dates)
	debits <- Map(Debits, debit_dates, lapply(x$debits, "[[", "amount"))
	Ledger(credits, debits)
}

# extend dates past some point if allowed; return dates
extend_dates <- function(x, beyond) UseMethod("extend_dates", x)
extend_dates.EntrySpec <- function(x, beyond) extend_dates(x$date, beyond)
extend_dates.Date <- function(x, beyond) x
extend_dates.UnexpandedDate <- function(x, beyond) {
	from <- as.Date(x$from)
	to <- if (!is.null(x$to)) {
		as.Date(x$to)
	} else {
		seq(from=beyond, by=x$by, length.out=2)[2]
	}
	seq(from, to, by=x$by)
}

Credits <- function(dates) structure(list(dates=dates), class=c("Credits", "Entry"))
Debits <- function(dates, amount) {
	list(dates=dates, amount=rep(amount, length.out=length(dates))) |>
	structure(class=c("Debits", "Entry"))
}

Ledger.list <- function(x, debits) {
	structure(list(credits=x, debits=debits), class="Ledger")
}


bound_capital <- function(ledger, savings) {
	costs <- sapply(ledger$debits, "[[", "amount")
	time_until_debits <- sapply(ledger$debits, "[[", "dates")
	origin <- if (savings == 0) { 0 } else {
		o <- nlm(linear_origin, 30,
		         S=savings, c=costs, t=time_until_debits,
		         iterlim=1E4)
		if (o$code > 2) stop("error locating origin") else o$estimate
	}
	browser()
	# TODO: review with green book
	savings_split <- savings_ratio(origin, costs, time_until_debits) *
		as.numeric((origin + diff(prior_credit_window)) / origin)
	Map(Debit,
	    name=next_debits$name,
	    cost=savings_split,
	    date=as.Date(end(prior_credit_window))) |>
	Debits()
}

# h: time since savings origin
# S: savings
# c: vector of costs
# t: time until each cost is incurred
linear_origin <- function(h, S, c, t) {
	t <- as.integer(t)
        gr <- function(h, S, c, t)
                2 *
                sum((h*c)/(h+t)^2 - c/(h+t)) *
                (S - sum(savings_ratio(h,c,t)))
        (S - sum(savings_ratio(h,c,t)))^2 |>
        structure(gradient = gr(h,S, c, t))
}
savings_ratio <- function(h, c, t) (h * c) / (h + t)

format.Debits <- function(x, display_date=TRUE, display_sum=TRUE,...) {
	fwidth <- nchar(format(sum(x))) + 5
	c(sapply(x, \(debit) {
		c(debit$name,
		sprintf(paste0("%", fwidth, ".2f"), debit$cost),
		if (display_date)
			sprintf("\t%s", format(debit$date)) else NULL)
	}),
	if (display_sum) {
		bar <- paste0(rep('-', max(nchar(x$name)) + 4), collapse='')
		c(bar, sprintf(paste0("Sum:%", fwidth-4, ".2f"), sum(x)), bar)
	} else NULL
	)
}

if (getOption("run.main", default=TRUE)) main()
