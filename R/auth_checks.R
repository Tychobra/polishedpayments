

is_subscription_required <- function(
  user_roles,
  prices = .pp$prices,
  free_roles = .pp$free_roles
) {

  if (is.null(prices) || length(intersect(user_roles, free_roles) > 0)) {
    out <- FALSE
  } else {
    out <- TRUE
  }

  out
}

is_subscription_valid <- function(stripe_user) {

  if (is.na(stripe_user$subscription)) {
    out <- FALSE
  } else if (!is.na(stripe_user$default_payment_method)) {
    out <- TRUE
  } else if (!is.na(stripe_user$trial_days_remaining) && stripe_user$trial_days_remaining > 0) {
    out <- TRUE
  } else {
    out <- FALSE
  }

  out
}