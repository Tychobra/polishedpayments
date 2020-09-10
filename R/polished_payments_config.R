


#' configure R options for Polished Payments
#'
#' @param .options a list containing your Stripe configuration.  The list should be
#' in the following format:
#' list(
#'   keys = list(
#'     secret: <your Stripe secret key>
#'     public: <your Stripe public key>
#'   ),
#'   prices = c(<character vector of 1 to 4 pricing options>)
#'   free_trial_days = <number of days for your free trial>
#' )
#'
#' @export
#'
#' @examples
#'
#' polished_payments_config(
#'   list(
#'     keys = list(
#'       secret: "fake_key_hjkasdhgkjashdkfj"
#'       public: "fake_key_hjaksdfhjaksldhfkj"
#'     ),
#'     prices = c("price_jkashdkfjh", "price_jakhkljgakwf")
#'     free_trial_days = 30
#'   )
#' )
#'
#'
polished_payments_config <- function(.options) {
  options("pp" = .options)
}
