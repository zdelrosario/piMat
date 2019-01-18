#' Re-express a given vector space in a target space
#'
#' @param df_given full rank matrix (thin or square)
#' @param df_target full rank matrix (thin or square)
#' @param ktol condition number stopping tolerance
#' @return square coefficient matrix
#' @export
#' @examples
#' df_given <-
#'   tribble(
#'     ~out_name, ~W1, ~W2,
#'          "X1",   1,  -1,
#'          "X2",   1,   0,
#'          "X3",   0,   1
#'   )
#'
#' df_target <-
#'   tribble(
#'    ~out_name, ~V1, ~V2,
#'         "X1",   1,   0,
#'         "X2",   1,   1,
#'         "X3",   0,   1
#'   )
#'
#' df_V2W <- convert_space(df_given, df_target)
#'
#' df_t <- matched_inner(df_target, transpose(df_V2W)) %>%
#'   mutate(out_name = df_target %>% names() %>% setdiff(., "out_name"))
#' transpose(df_t)

convert_space <- function(df_given, df_target, ktol = 1e+6) {
  ## Check invariants
  if (!("out_name" %in% names(df_given))) {
    stop("df_given does not contain an out_name column")
  }
  if (!("out_name" %in% names(df_target))) {
    stop("df_target does not contain an out_name column")
  }
  if (length(setdiff(df_given %>% pull(out_name), df_target %>% pull(out_name))) != 0) {
    stop("out_name columns in df_given and df_target do not match!")
  }
  if (length(setdiff(df_target %>% pull(out_name), df_given %>% pull(out_name))) != 0) {
    stop("out_name columns in df_given and df_target do not match!")
  }
  ## Convert to matrices
  M_given <-
    df_given %>%
    dplyr::arrange(out_name) %>%
    dplyr::select(-out_name) %>%
    as.matrix()
  M_target <-
    df_target %>%
    dplyr::arrange(out_name) %>%
    dplyr::select(-out_name) %>%
    as.matrix()
  ## Check condition numbers
  k_given = kappa(M_given)
  if (k_given > ktol) {
    stop("df_given not full rank; stopping.")
  } else if (k_given > 1e3) {
    warning("df_given is ill-conditioned")
  }
  k_target = kappa(M_target)
  if (k_target > ktol) {
    stop("df_target not full rank; stopping.")
  } else if (k_target > 1e3) {
    warning("df_target is ill-conditioned")
  }
  ## Convert between spaces
  fit <-
    lm(M_given ~ M_target + 0)
  ## Check condition

  ## Return
  fit$coefficients %>%
    as.tibble() %>%
    dplyr::mutate(out_name = df_target %>% names() %>% setdiff(., "out_name")) %>%
    dplyr::select(out_name, everything())
}
