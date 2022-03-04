ua <- httr::user_agent("http://github.com/tychobra/polishedpayments")

#' Polished Payments API - Get Customer(s)
#'
#' @param app_uid an optional app uid.
#' @param user_uid an optional user uid.
#' @param is_live whether or not the Stripe key is live or test mode.
#' @param api_key your Polished API key. Set your polished api key using \code{\link{set_api_key}()}
#' so that you do not need to supply this argument with each function call.
#'
#' @details If both the \code{app_uid} and \code{user_uid} are \code{NULL}, then all the
#' customers in your account will be returned.  If only \code{app_uid} is not \code{NULL},
#' then all customers for a single app will be returned.  If only \code{user_uid} is not
#' \code{NULL} then all customers for that user will be returned (a user can have multiple
#' customers for multiple apps).  If \code{app_uid} and \code{user_uid} are provided, then
#' one customer will be returned (assuming that customer exists). If the customer does not
#' exists, a zero row tibble will be returned.
#'
#' @return an object of class \code{polished_api_res}.  The "content" of the object is a
#' tibble of app(s) with the following columns:
#' - uid
#' - app_name
#' - app_url
#' - created_at
#' - modified_at
#'
#' @export
#'
#' @seealso [add_customer()] [update_customer()]
#'
#' @importFrom httr GET authenticate
#' @importFrom polished polished_api_res api_list_to_df get_api_key
#'
get_customers <- function(
  app_uid = NULL,
  user_uid = NULL,
  is_live = .pp$is_live,
  api_key = polished::get_api_key()
) {

  query_out <- list()
  query_out$app_uid <- app_uid
  query_out$user_uid <- user_uid
  query_out$is_live <- is_live

  resp <- httr::GET(
    url = paste0(.polished$api_url, "/customers"),
    ua,
    httr::authenticate(
      user = api_key,
      password = ""
    ),
    query = query_out
  )

  resp_out <- polished::polished_api_res(resp)

  resp_out$content <- polished::api_list_to_df(resp_out$content)

  resp_out
}


#' Polished Payments API - Add a Customer
#'
#' @param app_uid the app uid.
#' @param user_uid the user uid.
#' @param stripe_customer_id the Stripe customer id.
#' @param stripe_subscription_id the Stripe subscription id.
#'
#' @inheritParams get_customers
#'
#' @export
#'
#' @seealso [get_customers()] [update_customer()]
#'
#' @importFrom httr POST authenticate
#' @importFrom polished polished_api_res get_api_key
#'
add_customer <- function(
  app_uid,
  user_uid,
  stripe_customer_id,
  stripe_subscription_id = NULL,
  is_live = .pp$is_live,
  api_key = polished::get_api_key()
) {

  body_out <- list(
    app_uid = app_uid,
    user_uid = user_uid,
    stripe_customer_id = stripe_customer_id,
    is_live = is_live
  )

  body_out$stripe_subscription_id <- stripe_subscription_id

  resp <- httr::POST(
    url = paste0(.polished$api_url, "/customers"),
    ua,
    httr::authenticate(
      user = api_key,
      password = ""
    ),
    body = body_out,
    encode = "json"
  )

  polished::polished_api_res(resp)
}




#' Polished Payments API - Update a Customer
#'
#' @param customer_uid The polished payments customer UID.
#' @param stripe_subscription_id the Stripe subscription ID.
#' @param free_trial_days_remaining_at_cancel The number of days remaining in the customer's
#' free subscription trial at time of cancelation.
#' @param default_payment_method the Stripe payment method ID for the customer's
#' default payment method.
#' @param cancel_subscription boolean - whether or not to cancel the existing subscription.
#'
#' @inheritParams get_customers
#'
#' @export
#'
#' @seealso [get_customers()] [add_customer()]
#'
#' @importFrom httr PUT authenticate
#' @importFrom polished polished_api_res get_api_key
#'
update_customer <- function(
  customer_uid,
  stripe_subscription_id = NULL,
  free_trial_days_remaining_at_cancel = NULL,
  default_payment_method = NULL,
  cancel_subscription = FALSE,
  is_live = .pp$is_live,
  api_key = polished::get_api_key()
) {

  # required parameters
  body_out <- list(
    customer_uid = customer_uid,
    is_live = is_live,
    cancel_subscription = cancel_subscription
  )

  # optional parameters
  body_out$stripe_subscription_id <- stripe_subscription_id
  body_out$free_trial_days_remaining_at_cancel <- free_trial_days_remaining_at_cancel
  body_out$default_payment_method <- default_payment_method


  resp <- httr::PUT(
    url = paste0(.polished$api_url, "/customers"),
    ua,
    httr::authenticate(
      user = api_key,
      password = ""
    ),
    body = body_out,
    encode = "json"
  )

  polished::polished_api_res(resp)
}
