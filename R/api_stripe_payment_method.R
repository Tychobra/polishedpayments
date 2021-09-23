#' Set the Default Payment Method for a Customer
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

#' get default payment method from Stripe
#'
#' Retrieved a list of information about the polishedpayments customer's default
#' payment method.
#'
#' @param payment_method_id the Stripe payment method id.
#'
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

