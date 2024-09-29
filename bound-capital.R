#!/usr/bin/env Rscript

VERSION <- "1.0.0"
DOC <- paste0("bound-capital - Determine Bound Capital [version ", VERSION, "]

Usage: bound-capital [FILE] -a CAPITAL [-d DATE] [-p PLOTNAME]
       bound-capital [FILE] [-a CAPITAL] [-d DATE] -x
       bound-capital -h | -v

Determine the total bound capital at next credit event (current
date inclusive), based on current accumulated capital.
Outputs yaml to stdout listing the amounts bound at next credit.

Arguments:
	FILE  yaml file containing credit and debit info.
	      If '-' or missing, stdin is expected.
	-a CAPITAL --accumulated CAPITAL  Total accumulated capital.
	-d DATE --date DATE  Current Date. [default: Today]
	-p PLOTNAME --plot PLOTNAME Plot linear growth to svg.

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
	maybe_plot(accumulated_capital, opts$plot) |>
	bound_capital(accumulated_capital) |>
	write_output()
}

write_output <- function(x) {
	as.list(x) |> as.yaml(precision=2) |> cat(sep='\n')
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

Ledger <- function(x, ...) UseMethod("Ledger", x)
Ledger.character <- function(x, current_date) {
	yaml.load(x, handlers=list(credit=CreditSpec)) |>
	LedgerSpec() |>
	Ledger(current_date)
}
Ledger.LedgerSpec <- function(x, current_date) {
	lapply(x$credits, extend_dates, beyond=current_date) |>
	do.call(max, args=_) |>
	lapply(x$debits, extend_dates, beyond=_) -> debit_dates
	credit_dates <- lapply(x$credits, extend_dates,
	                       beyond=do.call(max, args=debit_dates))
	credits <- Map(list,
	               dates=credit_dates,
	               amount=lapply(x$credits, "[[", "amount")) |>
 	           Credits()
	debits <- Map(list,
	              dates=debit_dates,
	              amount=lapply(x$debits, "[[", "amount")) |>
 	           Debits()
	Ledger(list(credits=credits, debits=debits))
}
Ledger.list <- function(x) {
	stopifnot(inherits(x$debits, "Debits"),
	          inherits(x$credits, "Credits"))
	structure(x, class=c("LedgerList", "Ledger"))
}
Entry <- function(x) UseMethod("Entry", x)
Entry.Entry <- identity
Entry.list <- function(x) {
	stopifnot(is.list(x), all(names(x) %in% c("amount", "dates")))
	within(x, {
		if (!is.null(amount))
			amount <- rep(amount, length.out=length(dates))
	}) |>
	structure(class="Entry")
}
Entries <- function(x) {
	lapply(x, Entry) |>
	structure(class=c("EntriesList", "Entries"))
}
Dates <- function(x) UseMethod("Dates", x)
Dates.EntriesList <- function(x) {
	lapply(x, Dates) |>
	structure(class="DatesList")
}
Dates.Entry <- function(x) x$dates
Amount <- function(x) UseMethod("Amount", x)
Amount.EntriesList <- function(x) {
	lapply(x, Amount) |> structure(class="AmountsList")
}
Amount.Entry <- function(x) x$amount
unique.DatesList <- function(x, incomparables=FALSE) {
	do.call(c, args=x) |> unique()
}
Summary.DatesList <- function(..., na.rm=FALSE)
	do.call(.Generic, args=c(..., na.rm=na.rm))

read_input <- function(filepath) {
	ifelse(is.null(filepath), "stdin", filepath) |>
	readLines() |> paste(collapse='\n')
}
Credits <- function(x) UseMethod("Credits", x)
Credits.list <- function(x) Entries(x) |> Credits()
Credits.Entries <- function(x)
	structure(x, class=c("CreditsList", "Credits", class(x)))
Credits.LedgerList <- function(x) x$credits
Debits <- function(x) UseMethod("Debits", x)
Debits.list <- function(x) Entries(x) |> Debits()
Debits.Entries <- function(x)
	structure(x, class=c("DebitsList", "Debits", class(x)))
Debits.LedgerList <- function(x) x$debits
is.Debits <- function(x) inherits(x, "Debits")

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

prepare <- function(x, current_date) UseMethod("prepare", x)
prepare.Ledger <- function(x, current_date) {
	remove_historical(x, current_date) |>
	normalise() |>
	collapse() |>
	remove_successive_debits()
}
remove_historical <- function(x, current_date) UseMethod("remove_historical", x)
remove_historical.Ledger <- function(x, current_date) {
	future_credits <- remove_historical(Credits(x), current_date)
	next_credit <- Dates(future_credits) |> min()
	future_debits <- remove_historical(Debits(x), next_credit)
	Ledger(list(credits=future_credits, debits=future_debits))
}
remove_historical.Credits <- function(x, current_date) {
	NextMethod(x) |> Credits()
}
remove_historical.Debits <- function(x, current_date) {
	NextMethod(x) |> Debits()
}
remove_historical.EntriesList <- function(x, current_date) {
	arrange_entry_by_date(\(dates) dates >= current_date) |>
	Map(x) |>
	Entries()
}
remove_successive_debits <- function(x) UseMethod("remove_successive_debits", x)
remove_successive_debits.Ledger <- function(x) {
	Debits(x) |> sort() |> head(1) |>
	list(credits=Credits(x), debits=_) |>
	Ledger()
}
# arrange_fun takes "dates" argument
arrange_entry_by_date <- function(arrange) function(x, ...) {
	with(x, {
		i <- arrange(dates)
		Entry(list(dates=dates[i], amount=amount[i]))
	})
}
head.Credits <- function(x, n=6L, ...) NextMethod(x) |> Credits()
head.Debits <- function(x, n=6L, ...) NextMethod(x) |> Debits()
head.EntriesList <- function(x, n=6L, ...) lapply(x, head, n=n, ...) |> Entries()
head.Entry <- function(x, n=6L, ...) lapply(x, head, n=n, ...) |> Entry()
sort.Credits <- function(x, decreasing=FALSE, ...) NextMethod(x) |> Credits()
sort.Debits <- function(x, decreasing=FALSE, ...) NextMethod(x) |> Debits()
sort.EntriesList <- function(x, decreasing=FALSE, ...)
	lapply(x, sort, decreasing=decreasing, ...) |> Entries()
sort.Entry <- arrange_entry_by_date(order)

collapse <- function(x) UseMethod("collapse", x)
collapse.Ledger <- function(x) {
	Debits(x) |> collapse() |>
	list(credits=Credits(x), debits=_) |> Ledger()
}
collapse.DebitsList <- function(x)
	lapply(x, collapse) |> Debits()
collapse.Entry <- function(x) with(x, {
	tapply(amount, dates, sum, simplify=FALSE) |>
	unlist(use.names=FALSE) |>
	list(amount=_, dates=sort(unique(dates))) |>
	Entry()
})

normalise <- function(x, compare) UseMethod("normalise", x)
normalise.Ledger <- function(x) {
	debits <- normalise(Debits(x), unique(Dates(Credits(x))))
	credits <- normalise(Credits(x), max(Dates(debits)))
	Ledger(list(credits=credits, debits=debits))
}
normalise.DebitsList <- function(x, compare) {
	# What is the earliest credit that each debit immediately succeeds?
	lapply(Dates(x), findInterval, sort(compare)) |>
	Map(list, dates=_, amount=Amount(x)) |> Debits()

}
normalise.CreditsList <- function(x, compare) {
	Map(list, dates=seq(compare), amount=Amount(x)) |> Credits()
}

with_capital_growth <- function(f) function(x, accumulated_capital) {
	stopifnot(is.Debits(x))
	costs <- Amount(x) |> unlist()
	time_until_debits <- Dates(x) |> unlist()
	origin <- determine_origin(costs, time_until_debits, accumulated_capital)
	f(origin, costs, time_until_debits)
}

bound_capital <- function(x, accumulated_capital)
	UseMethod("bound_capital", x)
bound_capital.Ledger <- function(x, accumulated_capital) {
	bound_capital(Debits(x), accumulated_capital)
}
bound_capital.DebitsList <- with_capital_growth(
	\(origin, costs, time_until_debits)
		savings_ratio(origin + 1, costs, time_until_debits - 1)
)
maybe_plot <- function(x, accumulated_capital, plotname) {
	p <- with_capital_growth(\(origin, costs, time_until_debits)
		plot_growth(origin, costs, time_until_debits,names(Debits(x)), plotname))
	if (!is.null(plotname)) p(Debits(x), accumulated_capital)
	x
}
 

determine_origin <- function(costs, time_until_debits, savings) {
	if (savings == 0 || savings > sum(costs)) { 0 } else {
		o <- nlm(linear_origin, 30,
		         S=savings, c=costs, r=time_until_debits,
		         iterlim=1E4)
		if (o$code > 2) stop("error locating origin") else o$estimate
	}
}

# h: time since savings origin
# S: savings
# c: vector of costs/debits
# r: vector of time remaining until each cost is incurred
# LaTeX (NB: Makes sense drawn geometrically, and with plot):
# \begin{align}
# \frac{s_i}{c_i} &= \frac{h}{h + r_i} \\
# s_i &= \frac{c_i h}{h + r_i}\\
# S &= \sum s_i \\
#   &= \sum \frac{c_i h}{h + r_i}\\
# f(h) &= (S - \sum \frac{c_i h}{h + r})^2\\
# \frac{df}{dh} &= 2(\sum \frac{c_i}{h + r_i} + \frac{c_i h}{(h + r_i)^2})(S - \sum \frac{c_i h}{h + r_i})^2
# \end{align}
linear_origin <- function(h, S, c, r) {
        gr <- function(h, S, c, r)
                2 *
                sum((h*c)/(h+r)^2 - c/(h+r)) *
                (S - sum(savings_ratio(h,c,r)))
        (S - sum(savings_ratio(h,c,r)))^2 |>
        structure(gradient = gr(h, S, c, r))
}
savings_ratio <- function(h, c, r) (h * c) / (h + r)

plot_growth <- function(h, c, r, debit_names, filename) {
        svg(filename)
        plot(c(1.3*(-h), 1.2*max(r)), c(0, max(c)), type="n",
                xlab="Time (Credit Events)", ylab="Amount",
                main="Capital Growth toward Expenses with Capital Pivot")
        text(r, c, debit_names, adj=c(-0.1,1))
        for (i in seq(length(c))) lines(c(-h, r[i], r[i]), c(0, c[i], 0))
        current_savings <- savings_ratio(h, c, r)
        text(0, max(current_savings), "Pivot\n(Capital)", adj=c(1,-0.1))
        segments(0,0,0,max(current_savings))
        next_savings <- savings_ratio(h+1,c,r-1)
        text(1, max(next_savings), "Next Credit Event", adj=c(1.1,-0.1))
        abline(v=1,lty=2)
        dev.off()
}

if (getOption("run.main", default=TRUE)) main()
