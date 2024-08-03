# Ref green notebook for accompanying workings and diagrams

costs <- data.frame(
amount = c(2,4),
date = c(4,6)
)

curr_save <- 0

next_pay <- 1

savings_ratio <- function(h, c, t) (h * c) / (h + t)

next_pay <- 1
savings_required <- sum(savings_ratio(next_pay, costs$amount, costs$date - next_pay))

# assuming we're at the next pay day, with S now saved, how much do we need for the next pay day? First, work out the savings origin...

curr <- next_pay
S <- savings_required
f <- function(h, S, c, t) {
	gr <- function(h, S, c, t)
		2 *
		sum((h*c)/(h+t)^2 - c/(h+t)) *
		(S - sum(savings_ratio(h,c,t)))
	(S - sum(savings_ratio(h,c,t)))^2 |>
	structure(gradient = gr(h,S, c, t))
}

h <- nlm(f, 4, S=S, c=costs$amount, t=costs$date - curr)$estimate

# then, run savings ratios for all future costs

next_pay <- 2
savings_required <- sum(savings_ratio(next_pay-curr + h, costs$amount, costs$date - next_pay))
difference_in_savings <- savings_required - S
eq <- abs(difference_in_savings - S) < 1E-5

# consider what happens when we reach the final payday before the first cost
# Each cost should be pulled to its nearest prior payday.
# Assume we have $X saved from the prior payday. Then this final payday, we subtract the cost from the savings, and put that in the "cost" bin, and consider only the remainder of our savings from which to attain the savings origin with respect to future costs (exclusive of the cost that has just been subtracted)

# say we have the appropriate amount saved at T = 4
curr <- 4
currently_saved <- sum(savings_ratio(curr, costs$amount, costs$date - curr))

# we then run the subtraction...

curr_costs_i <- costs$date == curr
curr_costs <- costs[curr_costs_i,,drop=F]
costs <- costs[!curr_costs_i,]
savings_after_curr_costs <- currently_saved - sum(curr_costs$amount)

# and can calculate h with the remaining savings and future costs

h <- nlm(f, 1, S=savings_after_curr_costs,
	c=costs$amount, t=costs$date - curr)$estimate
total_to_save <- sum(savings_ratio(h, costs$amount, costs$date - curr))
total_payday_keep <- total_to_save + sum(curr_costs$amount)
