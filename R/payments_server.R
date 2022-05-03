


#' adds polished payments to your Shiny server
#'
#'
#' Wrap your Shiny server in this function to add polished payments to your Shiny
#' server before your custom Shiny app's server logic runs.
#'
#' @param server the Shiny server function
#'
#' @importFrom shiny reactiveVal observeEvent getQueryString updateQueryString
#' @importFrom shinyFeedback showToast
#' @importFrom httr status_code
#'
#' @export
#'
payments_server <- function(
  server
) {

  function(input, output, session) {

    session$userData$stripe <- reactiveVal(NULL)
    shiny::observeEvent(session$userData$user(), {

      query_list <- shiny::getQueryString()
      query_page <- query_list$page


      hold_user <- session$userData$user()
      is_on_payments <- identical(query_page, "payments")

      browser()
      stripe_out <- NULL
      if (!is.null(hold_user)) {
        tryCatch({
          # get any existing subscriptions from Polished API

          stripe_out <- get_stripe(
            user_uid = hold_user$user_uid
          )

        }, error = function(err) {

          print(err)

          if (identical(err$message, "subscription canceled")) {
            # you can't create a Stripe subscription until after the Stripe user
            # has entered their payment info.  If the stripe user needs to create
            # a subscription, send them to the payments page.
            shiny::updateQueryString(
              queryString = "?page=payments",
              session = session,
              mode = "replace"
            )
            session$reload()
          } else {

            shinyFeedback::showToast("error", err$message)
          }

          invisible(NULL)
        })
      }


      if (is_on_payments) {

        if (is.null(user)) {
          # go to registration.  User must register before they can create subscription
          shiny::updateQueryString(
            queryString = "?page=sign_in&register=TRUE",
            session = session,
            mode = "replace"
          )
          session$reload()

        } else {
          # go to polished payments
          session$userData$stripe(stripe_out)
          payments_app_server(input, output, session)
        }


      } else {

        if (isTRUE(.pp$is_subscription_required)) {

          if (is.null(user)) {
            # redirect to registration page
            shiny::updateQueryString(
              queryString = "?page=sign_in&register=TRUE",
              session = session,
              mode = "replace"
            )
            session$reload()

          } else if (is_subscription_valid(stripe_user)) {

            # Go to custom Shiny app using polishedpayments
            session$userData$stripe(stripe_out)
            server(input, output, session)

          } else {

            shiny::updateQueryString(
              queryString = "?page=payments",
              session = session,
              mode = "replace"
            )
            session$reload()

          }

        } else {

          # subscription is not required
          # Go to custom Shiny app using polishedpayments
          session$userData$stripe(stripe_out)
          server(input, output, session)

        }

      }

    }, ignoreNULL = polished::.polished$is_auth_required)

  }
}
