
#' create a Stripe subscription
#'
#' @param customer_id the user's Stripe customer ID.  The user must already be a Stripe
#' customer before you can create a Stripe subscription.
#' @param plan_to_enable the Stripe plan to enable.
#' @param days_remaining the number of free trial days remaining.
#' @param default_payment_methos the Stripe ID for the default credit card for the Stripe subscription.
#' Keep as NA if the user has not yet entered their credit card information.
#'
#' @return the ID of the newly created Stripe subscription
#'
#' @noRd
#'
create_stripe_subscription <- function(customer_id, plan_to_enable, days_remaining = 30, default_payment_method = NA) {

  post_body <- list(
    "customer" = customer_id,
    `items[0][plan]` = plan_to_enable
  )

  if (!is.na(default_payment_method)) {
    post_body$default_payment_method = default_payment_method
  }

  # if user has already created a free trial, and then canceled their free trial part way through,
  # we keep track of their free trial days used and send them with the create subscription request
  # so that the user does not get to completely restart their free trial.
  if (!is.na(days_remaining)) {
    post_body$trial_period_days <- floor(as.numeric(days_remaining))
  }

  # Create the subscription and attach Customer & payment method to newly created subscription
  res <- httr::POST(
    paste0("https://api.stripe.com/v1/subscriptions"),
    body = post_body,
    encode = "form",
    httr::authenticate(
      user = getOption("pp")$keys$secret,
      password = ""
    )
  )

  res_content <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )

  if (!identical(httr::status_code(res), 200L)) {

    print(res_content)
    stop("unable to create subscription", call. = FALSE)

  }

  # return the newly created subscription id
  res_content$id
}