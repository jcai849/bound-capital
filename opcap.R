# usage: 
# opcap determines how much needs to be saved at next pay.
# next pay is determined relative to current date (inclusive)
# savings: savings on day of pay (prior to receiving payment)

Debit <- function(name, cost, date) {
	stopifnot(is.character(name) && length(name) == 1)
	stopifnot(is.Date(date) && length(date))
	stopifnot(is.numeric(cost) &&
		length(cost) == 1 || length(cost) == length(date))
	list(name=name, cost=cost, date=date) |>
	structure(class="Debit")
}
is.Debit <- function(debit) inherits(debit, "Debit")
Debits <- function(...) {
	debits <- list(...)
	stopifnot(length(debits))
	stopifnot(all(sapply(debits, is.Debit)))
	structure(debits, class="Debits")
}
is.NA.Debits <- function(x) vapply(x, \(debit) length(debit$date) == 0, logical(1))
Summary.Debits <- function(..., na.rm) {
	list(...)[1] |> sapply(\(debit) .Generic(debit$cost)) |> .Generic()
}

main <- function(current_date, credits, debits, savings) {
	current_future_credits <- filter_historical_credits(current_date, credits)
	current_future_debits  <- filter_historical_debits(current_date, debits)

        current_debits_to_save <- current_debits(current_future_credits, current_future_debits)

	# TODO
	future_credits <- filter_current_credits(current_future_credits)
	future_debits <- filter_current_debits(current_date, debits)

	future_debits_to_save <- future_debits(future_credits, future_debits, savings - sum(current_debits_to_save))

	c(current_debits_to_save, future_debits_to_save)
}

filter_debits <- function(filter, debits) {
	i <- lapply(debits, filter)
	mapply(\(debit, i) { within(debit, {
		if (length(cost) > 1) cost <- cost[i]
		date <- date[i]
		}
	}, debits, i, SIMPLIFY=FALSE, USE.NAMES=FALSE)

}

filter_historical_credits <- function(current_date, credits) {
	out <- credits[credits < current_date]
	if (!length(out)) warning("credit expired")
	out
}
filter_historical_debits <- function(current_date, credits, debits) {
	current_future_debits <- filter_debits(\(debit) item$date >= current_date, debits)
	expired_i <- is.NA(current_future_debits)
	lapply(current_future_debits[expired_i], \(debit) warning(sprintf("%s expired", debit$name)))
	current_future_debits[!expired_i]
}

current_debits <- function(credits, debits) {
	if (length(credits) < 2) error("insufficient future credits provided")
	next_pay <- credits[1]
	next_next_pay <- credits[2]
	# get all items within next pay window
	current_debits <- filter_debits(\(debit) next_pay <= debit$date & debit$date < next_next_pay, debits)
	current_debits[!is.NA(current_debits)]
}

linear_origin <- function(h, S, c, t) {
	t <- as.integer(t)
        gr <- function(h, S, c, t)
                2 *
                sum((h*c)/(h+t)^2 - c/(h+t)) *
                (S - sum(savings_ratio(h,c,t)))
        (S - sum(savings_ratio(h,c,t)))^2 |>
        structure(gradient = gr(h,S, c, t))
}
savings_ratio <- function(h, c, t) (h * c) / (h + as.integer(t))
next_savings <- function(debit, savings)
	next_pay <-  min(credit$date)
	curr_costs_i <- debit$date == next_pay
	save <- cbind(debit[curr_costs_i,], save=debit[curr_costs_i,"cost"])
	savings_remaining <- max(0, savings - save$save)
	
	debit <- debit[!curr_costs_i,]
	if (!nrow(debit)) return(save)

	# if no savings remaining, set linear origin as "next pay - avg pay cycle length"
	req_save <- if (savings_remaining == 0) {
	# else go linear
	} else {
		h <- nlm(linear_origin, 1, S=savings_remaining, c=debit$cost, t=debit$date - next_pay)$estimate
		savings_ratio(next_pay + h, debit$cost, debit$date - next_pay)
	}
	rbind(save, cbind(debit, save=req_save))
}
