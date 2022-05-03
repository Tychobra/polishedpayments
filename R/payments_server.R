


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
      query_page <- query_list$page


      hold_user <- session$userData$user()
      is_on_payments <- identical(query_page, "payments")
      tryCatch({
        # get any existing subscriptions from Polished API

        stripe_out <- get_stripe(
          user_uid = hold_user$user_uid
        )



        if (!is_on_payments && isTRUE(.pp$is_subscription_required) && !is_subscription_valid(stripe_out)) {

          shiny::updateQueryString(
            queryString = "?page=payments",
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

    })



    if (isTRUE(.pp$is_subscription_required)) {
      ignore_null <- TRUE
    } else {
      ignore_null <- FALSE
    }
    observeEvent(session$userData$stripe(), {
      query_list <- shiny::getQueryString()
      page_query <- query_list$page

      if (identical(page_query, "payments")) {

        payments_app_server(input, output, session)

      } else {

        server(input, output, session)
      }

    }, once = TRUE, ignoreNULL = ignore_null)

  }
}
