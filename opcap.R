linear_origin <- function(h, S, c, t) {
        gr <- function(h, S, c, t)
                2 *
                sum((h*c)/(h+t)^2 - c/(h+t)) *
                (S - sum(savings_ratio(h,c,t)))
        (S - sum(savings_ratio(h,c,t)))^2 |>
        structure(gradient = gr(h,S, c, t))
}

savings_ratio <- function(h, c, t) (h * c) / (h + t)

next_savings <- function(credit, debit, savings) {
	# subtract current costs
	next_pay <-  min(credit$date)
	curr_costs_i <- debit$date == next_pay
	save <- cbind(debit[curr_costs_i,], save=debit[curr_costs_i,"cost"])
	savings_remaining <- max(0, savings - save$save)
	
	debit <- debit[!curr_costs_i,]
	if (!nrow(debit)) return(save)

	# if no savings remaining, go geometric
	req_save <- if (savings_remaining == 0) {
		credits_remaining <- vapply(debit$date, \(x) sum(credit$date <= x), numeric(1))
		debit$cost / credits_remaining
	# else go linear
	} else {
		h <- nlm(linear_origin, 1, S=savings_remaining, c=debit$cost, t=debit$date - next_pay)$estimate
		savings_ratio(next_pay + h, debit$cost, debit$date - next_pay)
	}
	rbind(save, cbind(debit, save=req_save)
}
