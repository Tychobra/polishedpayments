


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
    session$userData$stripe_trigger <- reactiveVal(0)

    shiny::observeEvent(session$userData$user(), {

      query_list <- shiny::getQueryString()
      payments_query <- query_list$payments

      hold_user <- session$userData$user()

      tryCatch({
        # get any existing subscriptions from Polished API

        stripe_out <- get_stripe(
          user_uid = hold_user$user_uid,
          user_roles = hold_user$roles,
          is_on_payments = identical(payments_query, "TRUE")
        )

        if (is.null(stripe_out)) {
          shiny::updateQueryString(
            queryString = "?payments=TRUE",
            session = session,
            mode = "replace"
          )
          session$reload()
        } else {
          session$userData$stripe(stripe_out)
        }



      }, error = function(err) {

        print(err)

        if (identical(err$message, "subscription canceled")) {
          # you can't create a Stripe subscription until after the Stripe user
          # has entered their payment info.  If the stripe user needs to create
          # a subscription (i.e. )
          shiny::updateQueryString(
            queryString = "?payments=TRUE",
            session = session,
            mode = "replace"
          )
          session$reload()
        } else {

          shinyFeedback::showToast("error", err$message)
        }

        invisible(NULL)
      })

    })




    observeEvent(session$userData$stripe(), {
      query_list <- shiny::getQueryString()
      payments_query <- query_list$payments

      if (identical(payments_query, "TRUE")) {

        payments_app_server(input, output, session)

      } else {

        server(input, output, session)
      }

    }, once = TRUE)

  }
}
