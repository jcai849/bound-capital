options(run.main=FALSE)
source("bound-capital.R")

dates <- 1:10
regular_amount <- 20
one_off_amount <- 20

credits <- Credits(list(salary=list(dates = dates, amount = NULL)))
debits <- Debits(list(regular=list(dates = dates, amount = regular_amount),
                      one_off=list(dates=max(dates), amount = one_off_amount)))
ledger <- Ledger(list(credits=credits, debits=debits))

next_bound_capital <- function(bound_cap, current_date) {
	current_ledger <- prepare(ledger, current_date)

	costs_at_prior_date <- sapply(bound_cap,
		\(x) if (x$dates < current_date) x$amount else 0)
	free_capital <- sum(bound_cap) - sum(costs_at_prior_date)

	b <- bound_capital(current_ledger, free_capital)
	Map(\(a,b) {Entry(list(dates=a$dates + current_date - 1, amount=b))},
	    Debits(current_ledger), b) |> Debits()
}

init <- Debits(list(starting=list(dates=1,amount=1)))
states <- Reduce(next_bound_capital, dates, init, accumulate=TRUE)
totals <- Map(sum, states) |> simplify2array()
print(diff(totals))
