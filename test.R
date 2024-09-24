#!/usr/bin/env Rscript

options(run.main=FALSE)
source("bound-capital.R")

#main(c("test.yaml", "-a", "132", "-d", "2024-01-14"))

origin <- -5
ledger <- Ledger(list(
	Credits(dates=1:3)),
	list(a=Debits(dates=1, amount=100),
	     b=Debits(dates=3, amount=100))
)
costs <- sapply(ledger$debits, "[[", "amount")
time_until_debits <- sapply(ledger$debits, "[[", "dates")
next_savings <- savings_ratio(1-origin, costs, time_until_debits-1)
current_savings <- savings_ratio(-origin, costs, time_until_debits)

x=bound_capital(ledger, sum(current_savings))
