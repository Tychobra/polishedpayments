

#' configure Polished Payments
#'
#' @param stripe_secret_key the Stripe secret key
#' @param stripe_public_key the Stripe publishable key
#' @param subscription_prices an unnamed character vector of Stripe price ids for your subscription.  e.g. monthly and
#' yearly pricing options.  A Stripe price id looks like this "price_64t6gq76vr78sdhf".  Set to \code{NULL}, the default,
#' if you are only using one time payments.
#' @param trial_period_days the number of days to offer for a free trial period.  All pricing options
#' will use this free trial period.  It overrides any free trial period set on your Stripe dashboard.
#' @param is_subscription_required \code{TRUE} or \code{FALSE} for whether or not a subscription is
#' required.  If there are no subscription prices passed to \code{subscription_prices} argument, then
#' this argument is always treated as \code{FALSE}.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' polished_payments_config(
#'   stripe_secret_key = "<your Stripe secret API key>",
#'   stripe_public_key = "<your Stripe publishable key>",
#'   subscription_prices = c("price_jkashdkfjh", "price_jakhkljgakwf"),
#'   trial_period_days = 30
#' )
#' }
#'
#'
polished_payments_config <- function(
  stripe_secret_key,
  stripe_public_key,
  subscription_prices = NULL,
  trial_period_days = 0,
  is_subscription_required = TRUE
) {

  if (!is.numeric(trial_period_days) && trial_period_days >= 0 && !is.null(subscription_prices)) {
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

  if (!(is.null(subscription_prices) || (is.character(subscription_prices) && length(subscription_prices) > 0))) {
    stop("`subscription_prices` must be a character vector or `NULL`", call. = FALSE)
  }

  if (!(
    is.logical(is_subscription_required) &&
    identical(length(is_subscription_required), 1L) &&
    !is.na(is_subscription_required)
  )) {
    stop("`is_subscription_required` must be `TRUE` or FALSE", call. = FALSE)
  }

  # force is_subscription_required to FALSE if no subscriptions provided
  if (is.null(subscription_prices) && isTRUE(is_subscription_required)) {
    warning("`is_subscription_required` cannot be `TRUE` when `subscription_prices` is NULL.  `is_subscription_required` set to `FALSE`.")
    is_subscription_required <- FALSE
  }

  assign("prices", subscription_prices, envir = .pp)
  assign("trial_period_days", trial_period_days, envir = .pp)
  assign("is_live", is_live, envir = .pp)
  assign("keys", list(
      secret = stripe_secret_key,
      public = stripe_public_key
    ),
    envir = .pp
  )
  assign("is_subscription_required", is_subscription_required, envir = .pp)

  invisible(NULL)
}

#' @export
.pp <- new.env()
