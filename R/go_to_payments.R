#' go_to_payments
#'
#' trigger event to redirect user to Account (payments) page,
#' typically used in an `observeEvent`
#'
#' @importFrom shiny getDefaultReactiveDomain updateQueryString
#'
#' @param session A Shiny session object.
#'
#' @export
#'

go_to_payments <- function(
  session = shiny::getDefaultReactiveDomain()
) {

  shiny::updateQueryString(
    queryString = "?page=payments",
    session = session,
    mode = "push"
  )

  session$reload()
}
