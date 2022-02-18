#' Create a new Stripe customer
#'
#' @param email The new Stripe customer's email address.
#' @param user_uid the new Stripe customer's polished user uid.
#'
#' @importFrom httr POST authenticate content status_code
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#'
create_stripe_customer <- function(
  email,
  user_uid,
  stripe_secret_key = .pp$keys$secret
) {

  res <- httr::POST(
    "https://api.stripe.com/v1/customers",
    body = list(
      "email" = email,
      "metadata[polished_uid]" = user_uid
    ),
    encode = "form",
    httr::authenticate(
      user = stripe_secret_key,
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



#' Collect information for a Stripe customer
#'
#' @param stripe_customer_id The Stripe customer's ID.
#'
#' @importFrom httr GET authenticate content status_code
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#'
get_stripe_customer <- function(
  stripe_customer_id,
  stripe_secret_key = .pp$keys$secret
) {

  res <- httr::GET(
    "https://api.stripe.com/v1/customers",
    query = list(
      "customer_id" = customer_id
    ),
    httr::authenticate(
      user = stripe_secret_key,
      password = ""
    )
  )

  res_data <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )

  if (!identical(httr::status_code(res), 200L)) {
    print(res_data)
    stop("error getting Stripe customer", call. = FALSE)
  }


  customer_id <- res_data$id
  if (is.null(customer_id)) {
    stop("no customer id received from Stripe")
  }

  res_data
}


