

#' Convert a list returned from the API back into a data frame
#'
#' We send data frames back from the API as lists.  This way NA columns are not
#' dropped when converting to and from JSON.
#'
#' @param api_list the data frame in list format as returned from the API
#'
api_list_to_df <- function(api_list) {

  return(tibble::as_tibble(api_list))
}
