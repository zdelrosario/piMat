#' Do a single circle-dot on a data frame
#'
#' @param weights inner product weights; vector
#' @param df_target target data; data frame
#' @return inner product results; vector
#' @export
#' @examples
#' weights <- c("X1" = +0.5, "X2" = -0.5)
#'
#' df_target <-
#'   tribble(
#'     ~X1, ~X2,
#'       4,   2,
#'       8,   6
#'   )
#'
#' named_cdot(weights, df_target)

named_cdot <- function(weights, df_given) {
  ## Return a matrix formed from the appropriate columns
  M <-
    df_given %>%
    dplyr::select(names(weights)) %>%
    as.matrix()
  ## Multiply each row elementwise
  t(t(M) * weights)
}
