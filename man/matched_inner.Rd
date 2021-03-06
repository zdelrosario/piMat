% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matched_inner.R
\name{matched_inner}
\alias{matched_inner}
\title{Do a batch of inner products on a data frame}
\usage{
matched_inner(df_weights, df_target)
}
\arguments{
\item{df_weights}{inner product weights}

\item{df_target}{target data frame}
}
\value{
data frame with inner product results
}
\description{
Do a batch of inner products on a data frame
}
\examples{
df_weights <-
  tribble(
    ~out_name, ~X2, ~X1,
         "Y1",   1,  -1,
         "Y2",   0,  -1
  )

df_target <-
 tibble(
   X1 = c( 0, 1, 2, 3),
   X2 = c( 0, 0, 1, 1),
   X3 = c( 5, 6, 7, 8)
 )

matched_inner(df_weights, df_target)

bind_cols(
  df_target,
  matched_inner(df_weights, df_target)
)
}
