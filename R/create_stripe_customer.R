

#' Create a new Stripe customer
#'
#' @param email The new Stripe customer's email address.
#' @param user_uid the new Stripe customer's polished user uid.
#'
#' @export
#'
#'
create_stripe_customer <- function(email, user_uid) {

  res <- httr::POST(
    "https://api.stripe.com/v1/customers",
    body = list(
      "email" = email,
      "metadata[polished_uid]" = user_uid
    ),
    encode = "form",
    httr::authenticate(
      user = getOption("pp")$keys$secret,
      password = ""
    )
  )

  res_data <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )

  if (!identical(httr::status_code(res), 200L)) {
    print(res_data)
    stop("error creating Stripe user", call. = FALSE)
  }


  customer_id <- res_data$id
  if (is.null(customer_id)) {
    stop("no customer id received from Stripe")
  }

  customer_id
}