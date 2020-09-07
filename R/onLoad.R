#' Adds the content of inst/assets/ to polishedpayments/
#'
#' @importFrom shiny addResourcePath registerInputHandler
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("polishedpayments", system.file("assets", package = "polishedpayments"))
}