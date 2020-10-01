#'
#' go_to_account
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

go_to_account <- function(
  session = shiny::getDefaultReactiveDomain()
) {

  shiny::updateQueryString(
    queryString = "?page=account",
    session = session,
    mode = "push"
  )

  session$reload()
}
