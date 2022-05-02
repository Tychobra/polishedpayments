


is_subscription_valid <- function(stripe_user) {

  if (is.na(stripe_user$subscription[1])) {
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