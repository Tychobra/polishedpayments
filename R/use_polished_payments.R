
#' Add the polished session data to your user
#'
#' Calling this function in your Shiny server will attached your user's Polished
#' payments data to a session$userData$payments() reactiveVal.
#'
#' @export
#'
#' @param session the Shiny session
#'
get_polished_payments <- function(polished_api_key, session = getDefaultReactiveDomain()) {

  if (is.null(polished:::.global_sessions)) {
    stop("polished must be configured to use polished payments")
  }

  session$userData$paymnts <- reactiveVal(NULL)

  observeEvent(session$userData$user(), {
    out <- NULL
    tryCatch({

      # get the user's subscription from the "subscriptions" table via the API
      res <- httr::GET(
        paste0(getOption("polished")$api_url, "/subscriptions"),
        httr::authenticate(
          user = getOption("polished")$api_key,
          password = ""
        ),
        encode = "json",
        body = list(
          app_uid = getOption("polished")$app_uid,
          user_uid = session$userData$user()$user_uid
        )
      )

      res_content <- jsonlite::fromJSON(
        httr::content(res, "text", encoding = "UTF-8")
      )

      if (!identical(res, 200L)) {
        print(res_content)
        stop()
      }

      if (!identical(length(res_content), 0)) {
        out <- as.list(res_content)
        session$userData$payments(out)
      }

    }, error = function(err) {
      print(err)
    })
  })




}

