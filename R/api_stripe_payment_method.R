#' Set the Default Payment Method for a Customer
#'
#' @param customer_id the Stripe customer id
#' @param payment_method_id the Stripe payment method id
#'
#' @importFrom httr POST authenticate content status_code
#' @importFrom jsonlite fromJSON
#'
#' @export
set_stripe_payment_method <- function(customer_id, payment_method_id) {
  res <- httr::POST(
    paste0("https://api.stripe.com/v1/customers/", customer_id),
    body = list(
      "invoice_settings[default_payment_method]" = payment_method_id
    ),
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
    stop("Unable to set default payment method", call. = FALSE)
  }

  res_content
}

#' get payment method information from Stripe for a particular Payment Method ID
#'
#' Retrieves a list of information about the `polishedpayment` customer's payment method.
#'
#' @param payment_method_id the Stripe payment method id.
#'
#' @importFrom httr GET authenticate content status_code
#' @importFrom jsonlite fromJSON
#'
get_stripe_payment_method <- function(payment_method_id) {

  res <- httr::GET(
    paste0("https://api.stripe.com/v1/payment_methods/", payment_method_id),
    encode = "form",
    httr::authenticate(
      user = getOption("pp")$keys$secret,
      password = ""
    )
  )

  res_dat <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )

  if (!identical(httr::status_code(res), 200L)) {
    msg <- "error getting default payment method"
    print()
    print(res_dat)
    stop(msg, call. = FALSE)
  }

  return(list(
    "payment_method_id" = res_dat$id,
    "name" = res_dat$billing_details$name,
    "address" = list(
      "city" = res_dat$billing_details$address$city,
      "line1" = res_dat$billing_details$address$line1,
      "line2" = res_dat$billing_details$address$line2,
      "postal_code" = res_dat$billing_details$address$postal_code,
      "state" = res_dat$billing_details$address$state
    ),
    "card_brand" = res_dat$card$brand,
    "card_last4" = res_dat$card$last4,
    "exp_month" = res_dat$card$exp_month,
    "exp_year" = res_dat$card$exp_year
  ))
}


#' create a Stripe payment
#'
#' @param amount a positive integer representing how much to charge in the
#' smallest currency unit (e.g., 100 cents to charge $1.00 or 100 to charge
#' ¥100, a zero-decimal currency)
#' @param customer_id the Stripe customer ID
#' @param payment_method_id the Stripe payment method ID
#' @param currency the currency
#' @param receipt_email the email address to receive the receipt
#' @param description payment description to show to user making payment
#' @param stripe_secret_key your Stripe account secret API key
#'
#' @importFrom httr POST content authenticate status_code
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
create_payment <- function(
  customer_id,
  payment_method_id,
  amount,
  currency = "usd",
  receipt_email = NULL,
  description = NULL,
  stripe_secret_key = getOption("pp")$keys$secret
) {


  body_out <- list(
    "customer" = customer_id,
    "payment_method" = payment_method_id,
    "amount" = amount,
    "currency" = currency,
    "payment_method_types[]" = "card",
    "confirm" = "true"
  )

  if (!is.null(receipt_email)) {
    body_out$receipt_email <- receipt_email
  }

  if (!is.null(description)) {
    body_out$description <- description
  }

  payment_confirmation_res <- httr::POST(
    "https://api.stripe.com/v1/payment_intents",
    encode = "form",
    httr::authenticate(
      user = stripe_secret_key,
      password = ""
    ),
    body = body_out
  )

  payment_confirmation <- jsonlite::fromJSON(
    httr::content(payment_confirmation_res, "text", encoding = "UTF-8")
  )

  if (!identical(httr::status_code(payment_confirmation_res), 200L)) {
    stop(payment_confirmation$error$message, call. = FALSE)
  }

  return(payment_confirmation)
}
