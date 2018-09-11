#' Transpose a data frame
#'
#' @param df_given input data frame with out_name
#' @return transposed data frame
#' @export
#' @examples
#' df_given <-
#'   tribble(
#'     ~out_name, ~X1, ~X2, ~X3,
#'          "W1",   1,   1,   0,
#'          "W2",  -1,   0,   1
#'   )
#'
#' transpose(df_given)

transpose <- function(df_given) {
  ## Check invariants
  if (!("out_name" %in% names(df_given))) {
    stop("df_given does not contain an out_name column")
  }
  ## Record names
  new_colnames <- df_given %>% pull(out_name)
  new_rownames <- df_given %>% names() %>% setdiff(., "out_name")
  ## Build output
  res <-
    df_given %>%
    select(-out_name) %>%
    as.matrix() %>%
    t() %>%
    as.tibble()
  colnames(res) <- new_colnames
  res %>%
    mutate(out_name = new_rownames) %>%
    select(out_name, everything())
}
