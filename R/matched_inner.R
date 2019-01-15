#' Do a batch of inner products on a data frame
#'
#' @param df_weights inner product weights
#' @param df_target target data frame
#' @return data frame with inner product results
#' @export
#' @examples
#' df_weights <-
#'   tribble(
#'     ~out_name, ~X2, ~X1,
#'          "Y1",   1,  -1,
#'          "Y2",   0,  -1
#'   )
#'
#' df_target <-
#'  tibble(
#'    X1 = c( 0, 1, 2, 3),
#'    X2 = c( 0, 0, 1, 1),
#'    X3 = c( 5, 6, 7, 8)
#'  )
#'
#' matched_inner(df_weights, df_target)
#'
#' bind_cols(
#'   df_target,
#'   matched_inner(df_weights, df_target)
#' )

matched_inner <- function(df_weights, df_target) {
  ## Check input invariants
  if (!("out_name" %in% names(df_weights))) {
    stop("df_weights does not contain an out_name column")
  }
  if (!all(names(df_weights %>% dplyr::select(-out_name)) %in% names(df_target))) {
    stop("df_weights contains variables not found in df_target")
  }
  ## Form matrices
  W <-
    df_weights %>%
    dplyr::select(-out_name) %>%
    as.matrix()
  M <-
    df_target %>%
    dplyr::select(names(df_weights %>% dplyr::select(-out_name))) %>%
    as.matrix()

  ## Form result
  res <- matrix(
    M %*% t(W),
    nrow = dim(M)[1],
    ncol = dim(W)[1]
  )
  colnames(res) <- df_weights %>% dplyr::pull(out_name)
  if ("out_name" %in% names(df_target)) {
    as.tibble(res) %>%
      dplyr::mutate(out_name = df_target %>% dplyr::pull(out_name)) %>%
      dplyr::select(out_name, dplyr::everything())
  } else {
    as.tibble(res)
  }
}
