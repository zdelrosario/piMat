#' Do a batch of power-products on a data frame
#'
#' @param df_weights power weights (exponents)
#' @param df_target target data frame
#' @return data frame with power-product results
#' @export
#' @examples
#' df_weights <-
#'   tribble(
#'     ~out_name, ~X1, ~X2,
#'          "Y1",   1,   1,
#'          "Y2",   1,  -1
#'   )
#'
#' df_target <-
#'  tibble(
#'    X1 = c( 1, 1, 1, 1),
#'    X2 = c( 2, 3, 4, 5)
#'  )
#'
#' matched_powprod(df_weights, df_target)
#'
#' bind_cols(
#'   df_target,
#'   matched_powprod(df_weights, df_target)
#' )

matched_powprod <- function(df_weights, df_target) {
  matched_inner(
    df_weights = df_weights,
    df_target = df_target %>%
      dplyr::mutate_if(is.numeric, log)
  ) %>%
    dplyr::mutate_if(is.numeric, exp)
}
