#' adds polished payments to your Shiny UI
#'
#'
#' @export
payments_ui <- function(
  ui
) {
  shiny::tagList(
    shiny::tags$head(
      tags$script(src = "https://js.stripe.com/v3"),
      tags$script(paste0("var stripe = Stripe('", getOption("pp")$keys$public, "');"))
    ),
    ui
  )
}
