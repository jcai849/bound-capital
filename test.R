#!/usr/bin/env Rscript
# given that opcap.R is sourced and a collection of debit(cost, date) and credit(saved,next_pay) objects and date == date
# when next_savings is run
# then the cost collection is returned with each cost having the correct amount to put aside for the next pay

source("opcap.R")
debit <- data.frame(name=c("groceries","event","event2"),
                    cost=c(13,100,1200),
                    date=as.Date(c("2024-01-01","2024-02-01","2024-02-15")))
credit <- list(date=as.Date(c("2024-01-01", "2024-01-15", "2024-02-01", "2024-02-15")))
savings <- 132

next_savings(credit, debit, savings) # -> amount, date, next_savings
