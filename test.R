#!/usr/bin/env Rscript

options(run.main=FALSE)
source("bound-capital.R")

debug(main)
main(c("test.yaml", "-a", "132", "-d", "2024-01-14"))
