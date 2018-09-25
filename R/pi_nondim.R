#' Compute a unique non-dimensionalizing factor
#'
#' @param u_dim target dimensions; named vector
#' @param df_dim dimension matrix
#' @return weights for non-dimensionalizing factor; named vector
#' @export
#' @examples
#' df_dim <-
#'   tribble(
#'     ~rho, ~U, ~d, ~mu, ~dim,
#'        1,  0,  0,   1,  "M",
#'       -3,  1,  1,  -1,  "L",
#'        0, -1,  0,  -1,  "T"
#'   )
#'
#' u_dim <- c(
#'   "M" = 1, "L" = 1, "T" = -2
#' )
#'
#' w_dim <- pi_nondim(u_dim, df_dim)
pi_nondim <- function(u_dim, df_dim) {
  ## Check invariants
  if (!(all(names(u_dim) %in% (df_dim %>% pull(dim))))) {
    stop("u_dim contains dimensions not found in df_dim")
  }
  ## Basis for pi subspace
  df_proj <-
    pi_projection(df_dim)
  ## Construct linear system
  df_tmp <-
    bind_rows(
      df_dim,
      df_proj
    )
  M <-
    df_tmp %>%
    select_if(is.numeric) %>%
    as.matrix()
  b <-
    c(u_dim, rep(0, dim(df_proj)[1]))
  ## Solve
  res <-
    lm(b ~ M + 0)

  names(res$coefficients) <-colnames(M)
  res$coefficients
}
