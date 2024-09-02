#!/usr/bin/env Rscript

# given that opcap.R is sourced and a collection of debit and credit objects and date == date
# when next_savings is run
# then the cost collection is returned with each cost having the correct amount to put aside for the next pay

source("opcap.R")
debug(next_savings)
debits <- Debits(
	Debit(name="groceries",
	     cost=c(190, 13, 56),
	     date=as.Date(c("2023-12-12", "2024-01-01", "2024-02-01"))),
	Debit(name="electricity",
	     cost=16,
	     date=as.Date(c("2024-01-01", "2024-01-14", "2024-02-01"))),
	Debit(name="event",
	     cost=130,
	     date=as.Date("2024-02-01")))

credits <- as.Date(c("2024-01-01", "2024-01-15", "2024-02-01", "2024-02-15"))
savings <- 132

main(as.Date("2024-01-01"), credits, debits, savings) # -> name, amount
