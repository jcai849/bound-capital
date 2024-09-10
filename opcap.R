# usage: 
# opcap determines how much needs to be saved at the outset of the current credit window.
# current date should match the beginning of one of the supplied credits
# savings: savings held immediately prior to outset of current credit window

Credits <- function(dates) {
	stopifnot(is.Date(dates) && as.logical(length(dates)))
	structure(dates, class=c("Credits", class(dates)))
}
Debit <- function(name, cost, date) {
	stopifnot(is.character(name) && as.logical(length(name)) == 1)
	stopifnot(is.Date(date) && as.logical(length(date)))
	stopifnot(is.numeric(cost) &&
		length(cost) == 1 || length(cost) == length(date))
	list(name=name, cost=cost, date=date) |>
	structure(class=c("Debit", "list"))
}
is.Debit <- function(debit) inherits(debit, "Debit")
Debits <- function(x, ...) UseMethod("Debits")
Debits.Debit <- function(x, ...) {
	c(list(x), list(...)) |> Debits()
}
Debits.list <- function(x, ...) {
	stopifnot(all(sapply(x, is.Debit)))
	structure(x, class=c("Debits", "list"))
}
is.empty <- function(x) UseMethod("is.empty", x)
is.empty.Debits <- function(x) vapply(x, is.empty, logical(1))
is.empty.Debit <- function(x) length(x$date) == 0
Summary.Debits <- function(..., na.rm) {
	x <- list(...)
	if (length(x) > 1) stop("summary not implemented for more than one argument")
	do.call(.Generic, list(x[[1]]$cost))
}
`%complement%` <- function(e1, e2) { 
	filter_debits(function(e1_debit) {
		e2_match_i <- match(e1_debit$name, e2$name)
		if (is.na(e2_match_i)) return(TRUE)
		! e1_debit$date %in% e2[[e2_match_i]]$date
	}, e1) }
head.Debits <- function(x, n=6L, ...) {
	lapply(x, head, n) |> Debits()
}
head.Debit <- function(x, n=6L, ...) within(x, {
		cost <- head(cost, n)
		date <- head(date, n)
	})
`$.Debits` <- function(x, name) lapply(x, `[[`, name) |> do.call(c, args=_)
`[.Debits` <- function(x, i) Debits(unclass(x)[i])

opcap <- function(current_date, credits, debits, savings) {
	if (!current_date %in% credits) stop("current date must match the outset of supplied credit windows")
	current_future_credits <- remove_historical_credits(current_date, credits)
	current_future_debits  <- remove_historical_debits(current_date, debits)

	current_credit_window <- head(current_future_credits, n=2)
        current_debits <- remove_future_debits(current_credit_window, current_future_debits)
	future_debits <- current_future_debits %complement% current_debits

	next_debits <- head(future_debits, n=1)
	debit_savings <- save_next_debits(next_debits, current_credit_window, savings - sum(current_debits))
	list(current=current_debits, future=debit_savings)
}

is.Date <- function(x) inherits(x, "Date")

filter_debits <- function(filter, debits) {
	i <- lapply(debits, filter)
	Map(\(debit, i) within(debit, {
		if (length(cost) > 1) cost <- cost[i]
		date <- date[i]
	}), debits, i) |>
	Debits()
}

remove_historical_credits <- function(current_date, credits) {
	out <- credits[credits >= current_date]
	if (!length(out)) stop("credit expired")
	out
}
remove_historical_debits <- function(current_date, debits) {
	current_future_debits <- filter_debits(\(debit)
		debit$date >= current_date, debits)
	expired_i <- is.empty(current_future_debits)
	lapply(current_future_debits[expired_i], \(debit)
		warning(sprintf("%s expired", debit$name)))
	current_future_debits[!expired_i]
}
remove_future_debits <- function(current_credit_window, debits) {
	if (length(current_credit_window) < 2)
		stop("insufficient future credits provided")
	current_debits <- filter_debits(\(debit)
		debit$date >= current_credit_window[1] &
		debit$date < current_credit_window[2],
		debits)
	current_debits[!is.empty(current_debits)]
}

save_next_debits <- function(next_debits, current_credit_window, current_savings) {
	costs <- next_debits$cost
	dates <- next_debits$date
	time_until_debits <- dates - current_credit_window[1]
	origin <- if (current_savings == 0) { 0 } else {
		o <- nlm(linear_origin, 30,
		         S=current_savings, c=costs, t=time_until_debits,
		         iterlim=1E4)
		if (o$code > 2) stop("error locating origin") else o$estimate
	}
	savings_split <- savings_ratio(origin, costs, time_until_debits) *
		as.numeric((origin + diff(current_credit_window)) / origin)
	Map(Debit,
	name=next_debits$name,
	cost=savings_split,
	date=current_credit_window[1]) |>
	Debits()
}

# h: time since savings origin
# S: current savings
# c: vector of costs
# t: time until each cost incurred
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
