
#' create a Stripe subscription
#'
#' @param customer_id the user's Stripe customer ID.  The user must already be a Stripe
#' customer before you can create a Stripe subscription.
#' @param plan_to_enable the Stripe plan to enable.
#' @param days_remaining the number of free trial days remaining.
#' @param default_payment_method the Stripe ID for the default credit card for the Stripe subscription.
#' Keep as \code{NULL} if the user has not yet entered their credit card information.
#'
#' @return the ID of the newly created Stripe subscription
#'
#' @importFrom httr POST authenticate content status_code
#' @importFrom jsonlite fromJSON
#'
#' @noRd
#'
create_stripe_subscription <- function(
  customer_id,
  plan_to_enable,
  days_remaining = 30,
  default_payment_method = NULL,
  stripe_secret_key = .pp$keys$secret
) {

  post_body <- list(
    "customer" = customer_id,
    `items[0][price]` = plan_to_enable
  )

  post_body$default_payment_method <- default_payment_method


  # if user has already created a free trial, and then canceled their free trial part way through,
  # we keep track of their free trial days used and send them with the create subscription request
  # so that the user does not get to completely restart their free trial.
  if (!is.na(days_remaining)) {
    post_body$trial_period_days <- floor(as.numeric(days_remaining))
  }

  # Create the subscription and attach Customer & payment method to newly created subscription
  res <- httr::POST(
    "https://api.stripe.com/v1/subscriptions",
    body = post_body,
    encode = "form",
    httr::authenticate(
      user = stripe_secret_key,
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


#' Get information on the user's Stripe subscription
#'
#' @param stripe_subscription_id Your user's Stripe subscription ID.
#'
#' @importFrom httr GET authenticate content status_code
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
get_stripe_subscription <- function(
  stripe_subscription_id,
  stripe_secret_key = .pp$keys$secret
) {

  res <- httr::GET(
    paste0("https://api.stripe.com/v1/subscriptions/", stripe_subscription_id),
    encode = "form",
    httr::authenticate(
      user = stripe_secret_key,
      password = ""
    )
  )

  res_content <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )


  status <- httr::status_code(res)



  if (!identical(status, 200L)) {
    print(res_content)
    stop("error getting Stripe subscription", call. = FALSE)
  }
  if (res_content$status == "canceled") {
    print("subscription canceled")
    out <- NA
  } else {
    out <- list(
      id = res_content$id,
      item_id = res_content$items$data$id,
      item_created = res_content$items$data$created, # in seconds
      plan_id = res_content$plan$id,
      default_payment_method = res_content$default_payment_method,
      nickname = res_content$plan$nickname,
      amount = res_content$plan$amount,
      currency = res_content$plan$currency,
      start_date = res_content$start_date,
      # cannot use default trial_period_days because those days do not update
      # based on the value passed to the "trial_period_days" body parameter when
      # the subscription is created
      trial_end = res_content$trial_end,
      interval = res_content$plan$interval
    )

    out <- lapply(out, function(x) ifelse(is.null(x), NA, x))

    # check if the item is still in it's free trial period.  If in free trial period,
    # calculate the number of trial days remaining.
    trial_days_remaining <- max(
      (out$trial_end - as.integer(Sys.time())) / 60 / 60 / 24,
      0,
      na.rm = TRUE
    )

    out$trial_days_remaining <- trial_days_remaining
  }

  out
}
