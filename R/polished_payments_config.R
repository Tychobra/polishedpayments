


#' configure R options for Polished Payments
#'
#' @param stripe_secret_key the Stripe secret key
#' @param stripe_public_key the Stripe publishable key
#' @param stripe_prices an unnamed character vector of Stripe price ids for your subscription.  e.g. monthly and
#' yearly pricing options.  A Stripe price id looks like this "price_64t6gq76vr78sdhf".
#' @param trial_period_days the number of days to offer for a free trial period.  All pricing options
#' will use this free trial period.  It overrides any free trial period set on your Stripe dashboard.
#' @param free_roles Polished user roles that can bypass having to set up a subscription and get free
#' access to your Shiny app.  This is often used to give certain users (e.g. your beta testers) free
#' access to your app.  Go to \url{https://dashboard.polished.tech} to create a user role
#' and add that role to specific users, or use the `polished` package's API wrapper functions
#' (`polished::create_role` & `polished::add_user_role`).
#'
#' @export
#'
#' @examples
#'
#' polished_payments_config(
#'   stripe_secret_key = "<your Stripe secret API key>",
#'   stripe_public_key = "<your Stripe publishable key>",
#'   stripe_prices = c("price_jkashdkfjh", "price_jakhkljgakwf"),
#'   trial_period_days = 30,
#'   free_roles = "free_user"
#' )
#'
#'
#'
polished_payments_config <- function(
  stripe_secret_key,
  stripe_public_key,
  stripe_prices,
  trial_period_days = 0,
  free_roles = character(0)
) {

  if (!is.numeric(trial_period_days) && trial_period_days >= 0) {
    stop("`trial_period_days` must be a number >= 0", call. = FALSE)
  }

  # check if Stripe keys are "live" or "test"
  if (identical(substr(stripe_secret_key, 1, 7), "sk_live")) {
    is_live <- TRUE
  } else if (identical(substr(stripe_secret_key, 1, 7), "sk_test")) {
    is_live <- FALSE
  } else {
    stop("invalid Stripe API keys", call. = FALSE)
  }

  options("pp" = list(
    keys = list(
      secret = stripe_secret_key,
      public = stripe_public_key
    ),
    prices = stripe_prices,
    trial_period_days = trial_period_days,
    free_roles = free_roles,
    is_live = is_live
  ))
}
