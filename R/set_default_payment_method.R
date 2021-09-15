#' Set the Default Payment Method for a Customer
#'
#' @export
set_default_payment_method <- function(customer_id, payment_method_id) {
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
