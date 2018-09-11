#' Do a single inner product on a data frame
#'
#' @param weights inner product weights; vector
#' @param df_target target data; data frame
#' @return inner product results; vector
#' @export
#' @examples
#' weights <- c(
#'   "X1" = 0.5,
#'   "X2" = 0,
#'   "X3" = -0.5
#' )
#'
#' df_target <-
#'  tibble(
#'    X1 = c( 0, 1, 2, 3),
#'    X2 = c( 0, 0, 1, 1),
#'    X3 = c( 5, 6, 7, 8)
#'  )
#'
#' named_inner(weights, df_target)

named_inner <- function(weights, df_given) {
  ## Return a matrix formed from the appropriate columns
  M <-
    df_given %>%
    dplyr::select(names(weights)) %>%
    as.matrix()
  ## Carry out inner product
  c(M %*% weights)
}
