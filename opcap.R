# usage: 
# opcap determines how much needs to be saved at next pay.
# next pay is determined relative to current date (inclusive)
# savings: savings on day of pay (prior to receiving payment)

Credits <- function(dates) {
	stopifnot(is.Date(dates) && length(dates))
	structure(dates, class=c("Credits", class(dates)))
}
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
Ops.Debits <- function(e1, e2) {
	if (.Generic != '-') stop("Operation not yet implemented")
	e1[!e1$name %in% e2$name]
}
head.Debits <- function(x, n=6L, ...) {
	lapply(x, head, n) |> Debits()
}
head.Debit <- function(x, n=6L, ...) within(x, {
		cost <- head(cost, n)
		date <- head(date, n)
	})
`$.Debits` <- function(x, name) sapply(x, `$`, name)
`$.Debits<-` <- function(x, name, value) {
	Map(\(x,v) {
		x[name] <- value
		x
	}, x, value) |>
	Debits()
}

main <- function(current_date, credits, debits, savings) {
	current_future_debits  <- remove_historical_debits(current_date, debits)
	current_future_credits <- remove_historical_credits(current_date, credits)
        current_debits <- remove_future_debits(current_future_credits, current_future_debits)
	future_debits <- current_future_debits - current_debits

	next_credit <- current_future_credits[1]
	next_debits <- head(future_debits, n=1)
	debit_savings <- save_next_debits(next_debits, next_credit, savings - sum(current_debits))
	list(current=current_debits, future=debit_savings)
}

filter_debits <- function(filter, debits) {
	i <- lapply(debits, filter)
	Map(\(debit, i) { within(debit, {
		if (length(cost) > 1) cost <- cost[i]
		date <- date[i]
		}
	}, debits, i)
}

remove_historical_credits <- function(current_date, credits) {
	out <- credits[credits < current_date]
	if (!length(out)) warning("credit expired")
	out
}
remove_historical_debits <- function(current_date, credits, debits) {
	current_future_debits <- filter_debits(\(debit) item$date >= current_date, debits)
	expired_i <- is.NA(current_future_debits)
	lapply(current_future_debits[expired_i], \(debit) warning(sprintf("%s expired", debit$name)))
	current_future_debits[!expired_i]
}
remove_future_debits <- function(credits, debits) {
	if (length(credits) < 2) error("insufficient future credits provided")
	next_pay <- credits[1]
	next_next_pay <- credits[2]
	# get all items within next pay window
	current_debits <- filter_debits(\(debit) next_pay <= debit$date & debit$date < next_next_pay, debits)
	current_debits[!is.NA(current_debits)]
}

save_future_debits <- function(next_debits, next_credit, current_savings) {
	costs <- next_debits$cost
	dates <- next_debits$date
	origin <- ifelse(savings_remaining == 0, current_credit,
	           nlm(linear_origin, 1, S=current_savings, c=costs, t=dates - next_credit)$estimate)
	amounts <- savings_ratio(next_pay + origin, costs, dates - next_pay)
	next_debits$date <- next_credit
	next_debits$cost <- amounts
	next_debits
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
