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
	q("no")
	if (opts$expand) { cat(ledger); quit("no") }
	capital <- as.numeric(opts$accumulated)
	opcap(current_date, credits, debits, capital) |>
	format(display_date=FALSE) |> cat(sep='\n')
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
	credits_i <- sapply(x, class) == "CreditSpec"
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
	lapply(x$debits, extend_dates, beyond=_) -> debit_dates
	latest_debit <- max(vapply(debit_dates, max, Sys.Date()))
	credit_dates <- lapply(x$credits, extend_dates, beyond=latest_debit)
	credits <- Map(Credit, credit_dates)
	debits <- Map(Debit, debit_dates, lapply(x$debits, "[[", "cost"))
	Ledger(credits, debits)
}

# extend dates past some point if allowed; return dates
extend_dates <- function(x, beyond) UseMethod("extend", x)
extend_dates.EntrySpec <- function(x, beyond) extend_dates(x$date, beyond)
extend_dates.Date <- function(x, beyond) item
extend_dates.UnexpandedDate <- function(x, beyond) {
	from <- as.Date(x$from)
	to <- if (!is.null(x$to)) {
		as.Date(x$to)
	} else {
		seq(beyond, by=x$by, length.out=2)[2]
	}
	seq(from, to, by=x$by)
}

Credits <- function(dates) structure(list(dates=dates), class="Credits")
Debits <- function(dates, cost) {
	list(dates=dates, cost=rep(cost, length.out=length(dates))) |>
	structure(class="Debits")
}

Ledger.Credits <- function(x, debits) {
	structure(list(credits=x, debits=debits), class="Ledger")
}

opcap <- function(current_date, credits, debits, savings) {
	# Remove debits from prior credit window
	prior_credit_window <- filter_prior_credit_window(current_date, credits)
	prior_credit_window_debits <- extract_debits(from=prior_credit_window, debits)
	adjusted_savings <- savings - sum(prior_credit_window_debits)

	# Pivot on prior credit (end of prior credit window == next credit)
	remove_historical_debits(from=end(prior_credit_window), debits) |>
	head(n=1) |>
	save_next_debits(prior_credit_window, adjusted_savings)
}

Credits.Date <- function(x, ...) {
	stopifnot(as.logical(length(x)))
	structure(x, class=c("Credits", "Date"))
}
CreditWindow <- function(start, end)
	c(start=start, end=end) |>
	structure(class=c("CreditWindow", "Date"))
start <- function(x) x["start"]
end <- function(x) x["end"]
Debit <- function(name, cost, date, ...) {
	stopifnot(is.character(name) && as.logical(length(name)) == 1)
	stopifnot(as.logical(length(date)))
	stopifnot(is.numeric(cost) &&
		length(cost) == 1 || length(cost) == length(date))
	list(name=name, cost=cost, date=as.Date(date)) |>
	structure(class=c("Debit", "list"))
}
is.Debit <- function(debit) inherits(debit, "Debit")
Debits.Debit <- function(x, ...) {
	c(list(x), list(...)) |> Debits()
}
Debits.list <- function(x, ...) {
	if (all(sapply(x, is.Debit))) { structure(x, class=c("Debits", "list"))
	} else lapply(x, \(debit) do.call(Debit, debit)) |> Debits()
}
is.empty <- function(x) UseMethod("is.empty", x)
is.empty.Debits <- function(x) vapply(x, is.empty, logical(1))
is.empty.Debit <- function(x) length(x$date) == 0
Summary.Debits <- function(..., na.rm) {
	x <- list(...)
	if (length(x) > 1) stop("summary not implemented for more than one argument")
	do.call(.Generic, list(x[[1]]$cost))
}
head.Debits <- function(x, n=6L, ...) {
	lapply(x, head, n) |> Debits()
}
head.Debit <- function(x, n=6L, ...) within(x, {
		cost <- head(cost, n)
		date <- head(date, n)
	})
`$.Debits` <- function(x, name) lapply(x, `[[`, name) |> do.call(c, args=_)
`[.Debits` <- function(x, i) Debits(unclass(x)[i])


is.Date <- function(x) inherits(x, "Date")

filter_debits <- function(filter, debits) {
	i <- lapply(debits, filter)
	Map(\(debit, i) within(debit, {
		if (length(cost) > 1) cost <- cost[i]
		date <- date[i]
	}), debits, i) |>
	Debits()
}

filter_prior_credit_window <- function(current_date, credits) {
	next_credit_i <- which.max(current_date <= credits)
	CreditWindow(start=credits[next_credit_i - 1],
	             end=credits[next_credit_i])

}
extract_debits <- function(from, debits) {
	extracted_debits <- filter_debits(\(debit)
		debit$date >= start(from) &
		debit$date < end(from),
		debits)
	extracted_debits[!is.empty(extracted_debits)]
}

remove_historical_debits <- function(from, debits) {
	current_future_debits <- filter_debits(\(debit)
		debit$date >= from, debits)
	expired_i <- is.empty(current_future_debits)
	lapply(current_future_debits[expired_i], \(debit)
		warning(sprintf("%s expired", debit$name), call.=F))
	current_future_debits[!expired_i]
}

save_next_debits <- function(next_debits, prior_credit_window, savings) {
	costs <- next_debits$cost
	dates <- next_debits$date
	time_until_debits <- dates - start(prior_credit_window)
	origin <- if (savings == 0) { 0 } else {
		o <- nlm(linear_origin, 30,
		         S=savings, c=costs, t=time_until_debits,
		         iterlim=1E4)
		if (o$code > 2) stop("error locating origin") else o$estimate
	}
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
savings_ratio <- function(h, c, t) (h * c) / (h + as.integer(t))

expand <- function(credits, debits) {
	names(debits) <- debits$name
	c(list(credit=credits),
	  lapply(debits, '[', c("cost", "date"))) |>
	as.yaml(handlers=list(Date=format))

}
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
