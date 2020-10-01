#'
#' accountPageTrigger
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

accountPageTrigger <- function(
  session = shiny::getDefaultReactiveDomain()
) {

  shiny::updateQueryString(
    queryString = "?page=test",
    session = session,
    mode = "replace"
  )

  session$reload()
}
