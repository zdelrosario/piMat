context("Pi-subspace non-dimensionalization")
library(piMat)

## Setup
df_dim1 <-
  tribble(
    ~rho, ~U, ~d, ~mu, ~dim,
       1,  0,  0,   1,  "M",
      -3,  1,  1,  -1,  "L",
       0, -1,  0,  -1,  "T"
  )

df_dim2 <-
  tribble(
    ~a, ~b, ~c, ~d, ~dim,
     1,  1,  1,  1,  "L"
  )

u_dim1 <- c("M" = 1, "L" = 1, "T" = -2)
u_dim2 <- c("T" = -2, "L" = 1, "M" = 1)
u_dim3 <- c("M" = 1, "L" = 1, "T" = 1, "Theta" = 1)

u_dim4 <- c("L" = 1)
res4 <- c("a" = 0.25, "b" = 0.25, "c" = 0.25, "d" = 0.25)

## Tests
test_that(
  "Invariants checked",
  {
    expect_error(
      pi_nondim(u_dim3, df_dim1),
      "u_dim contains dimensions not found in df_dim"
    )
  }
)

test_that(
  "Re-ordering functions as intended",
  {
    expect_equal(
      pi_nondim(u_dim1, df_dim1),
      pi_nondim(u_dim2, df_dim1)
    )
  }
)

test_that(
  "Works for rank-deficient df_dim",
  {
    expect_equal(
      pi_nondim(u_dim4, df_dim2),
      res4
    )
  }
)
