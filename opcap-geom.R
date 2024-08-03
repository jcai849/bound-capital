#!/usr/bin/env Rscript

current_savings <- 1234  + 202.67 + 61.54 + 82.74 + 119.05 + 200
buffer <- 1000

# periodic costs
# periodic income

# accounts
df <- data.frame(account=c("mortgage", "electricity", "insurance"),
	income_until_cost=c(4,1,5),
	cost=c(2000,100,1500)
)
df <- within(df, saving <- cost / income_until_cost)
print(format(df))
required_savings <- sum(df$saving) + buffer - current_savings
sprintf("$%.2f required as saving at next income for operating costs and $%.2f buffer at current savings amount of $%.2f\n",
	required_savings, buffer, current_savings) |> cat()
