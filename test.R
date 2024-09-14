#!/usr/bin/env Rscript

# given that opcap.R is sourced and a collection of debit and credit objects and date == date
# when next_savings is run
# then the cost collection is returned with each cost having the correct amount to put aside for the next pay

source("opcap.R")

debits <- Debits("test-expenses.yml")
Debits(Debit(name="groceries",
	     cost=c(10L, 13L, 56L, 100L),
	     date=as.Date(c("2023-12-12", "2024-01-01", "2024-02-01", "2024-02-04"))),
	Debit(name="electricity",
	     cost=16L,
	     date=as.Date(c("2024-01-01", "2024-01-14", "2024-02-01"))),
	Debit(name="event1",
	     cost=130L,
	     date=as.Date(c("2024-02-05", "2024-02-06"))),
	Debit(name="event2",
	     cost=100L,
	     date=as.Date(c("2024-02-04", "2024-02-05")))) |>
identical(debits) |> stopifnot()

credits <- Credits(as.Date(c("2023-01-01", "2023-12-01", "2024-01-01", "2024-01-15", "2024-02-01", "2024-02-05")))
savings <- 132
current_date <- as.Date("2024-01-01")

print("working")
opcap(current_date, credits, debits, savings) |> str()

# try when some debits expired
print("failing")
tryCatch(opcap(as.Date("2024-02-05"), credits, debits, savings), error=\(e) e)
