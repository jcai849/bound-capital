#!/usr/bin/env Rscript

options(run.main=FALSE)
source("opcap.R")

current_date <- as.Date("2024-01-14")
input <- yaml.load_file("test.yaml") |> structure(class="yaml")
debits <- Debits(input, current_date)
credits <- Credits(input, current_date)
savings <- 132

print("working")
opcap(current_date, credits, debits, savings) |> str()

# try when some debits expired
print("failing")
current_date <- as.Date("2024-03-05")
debits <- Debits(input, current_date)
credits <- Credits(input, current_date)
tryCatch(opcap(current_date, credits, debits, savings), error=\(e) e)
