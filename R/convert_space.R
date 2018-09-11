#' Re-express a given vector space in a target space
#'
#' @param df_given
#' @param df_target
#' @return
#' @export
#' @examples
#' df_given <-
#'   tribble(
#'     ~out_name,
#'   )
#'

convert_space <- function(df_given, df_target) {
  ## Check invariants
  if (!("out_name" %in% names(df_given))) {
    stop("df_given does not contain an out_name column")
  }
  if (length(setdiff(names(df_given), names(df_target))) != 0) {
    stop("Variables in df_given and df_target do not match!")
  }
  if (dim(df_given)[1] != dim(df_target)[1]) {
    stop("Number of vectors (rows) in df_given and df_target do not match!")
  }
  ## Convert to matrices
  M_given <-
    df_given %>%
    select(names(df_target)) %>%
    as.matrix()
  M_target <-
    df_target %>%
    as.matrix()
  ## Convert between spaces
  res <-
    lm(M_given ~ M_target + 0)
  ## Check condition

  ## Return
  res$coefficients
}
