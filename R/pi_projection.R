#' Generate a projection to the pi-subspace
#'
#' @param df_dim dimension matrix
#' @return data frame for matched_inner()
#' @export
#' @examples
#' ## Basic example
#' df_dim <-
#'   tribble(
#'     ~rho, ~U, ~d, ~mu, ~dim,
#'        1,  0,  0,   1,  "M",
#'       -3,  1,  1,  -1,  "L",
#'        0, -1,  0,  -1,  "T"
#'   )
#'
#' df_pi_proj <-
#'   pi_projection(df_dim)
#'
#' df_target <-
#'   tribble(
#'     ~rho, ~U, ~d, ~mu,
#'      1.2, 10,  5, 1e-5,
#'      1.2, 20,  5, 1e-5,
#'      1.2, 10, 10, 1e-5,
#'      1.2, 20, 10, 1e-5
#'   )
#'
#' matched_inner(df_pi_proj, df_target)
#'
#' ## Find basis orthogonal to given direction
#' df_dim2 <-
#'   tribble(
#'     ~a, ~b, ~c, ~d, ~e, ~dim,
#'      1,  1,  1,  1,  1,  "L",
#'      1, -1,  0,  0,  0,  ""
#'   )
#'
#' df_pi_proj2 <-
#'   pi_projection(df_dim2)

pi_projection <- function(df_dim) {
  df_working <-
    df_dim %>%
    dplyr::select_if(is.numeric) # Remove label column
  ## Compute basis
  D <- as.matrix(df_working)
  res <- qr(t(D))
  rank <- res$rank
  V <- t(qr.Q(res, complete = TRUE)[, -(1:rank)])

  if (rank <= 0) {
    stop("pi-subspace is full-dimensional")
  } else if (rank >= dim(D)[2]) {
    stop("pi-subspace is zero-dimensional")
  } else {
    colnames(V) <- names(df_working)
    names_pi <- stringr::str_c("pi", 1:dim(V)[1])
    as.tibble(V) %>%
      dplyr::mutate(out_name = names_pi) %>%
      dplyr::select(out_name, tidyselect::everything())
  }
}
