% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_space.R
\name{convert_space}
\alias{convert_space}
\title{Re-express a given vector space in a target space}
\usage{
convert_space(df_given, df_target, ktol = 1e+06)
}
\arguments{
\item{df_given}{full rank matrix (thin or square)}

\item{df_target}{full rank matrix (thin or square)}

\item{ktol}{condition number stopping tolerance}
}
\value{
square coefficient matrix
}
\description{
Re-express a given vector space in a target space
}
\examples{
df_given <-
  tribble(
    ~out_name, ~W1, ~W2,
         "X1",   1,  -1,
         "X2",   1,   0,
         "X3",   0,   1
  )

df_target <-
  tribble(
   ~out_name, ~V1, ~V2,
        "X1",   1,   0,
        "X2",   1,   1,
        "X3",   0,   1
  )

df_V2W <- convert_space(df_given, df_target)

df_t <- matched_inner(df_target, transpose(df_V2W)) \%>\%
  mutate(out_name = df_target \%>\% names() \%>\% setdiff(., "out_name"))
transpose(df_t)
}
