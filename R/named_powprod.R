#' Do a single power-product on a data frame
#'
#' @param weights power weights (exponents); vector
#' @param df_target target data; data frame
#' @return inner product results; vector
#' @export
#' @examples
#' weights <- c(
#'   "X1" = +1,
#'   "X2" = -1,
#' )
#'
#' df_target <-
#'  tibble(
#'    X1 = c( 1, 1, 1.0),
#'    X2 = c( 2, 3, 0.5),
#'  )
#'
#' named_inner(weights, df_target)

named_powprod <- function(weights, df_given) {
  ## Return a matrix formed from the appropriate columns
  M <-
    df_given %>%
    dplyr::select(names(weights)) %>%
    dplyr::mutate_if(is.numeric, log) %>%
    as.matrix()
  ## Carry out inner product
  exp(c(M %*% weights))
}
