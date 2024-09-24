#!/usr/bin/env Rscript

options(run.main=FALSE)
source("bound-capital.R")

test_main <- function() {
	main(c("test.yaml", "-a", "132", "-d", "2024-01-14"))
}

test_bound_capital <- function() {
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
	bound_capital(ledger, sum(current_savings))
}

test_plot <- function() {
	h <- 5
	c <- c(3,4,7,2)
	r <- c(2,3,4,6)
	debit_names <- letters[1:4]
	plot_growth(h,c,r,debit_names,"test.svg")
}

test_bound_capital()
