context("Pi-subspace projection")
library(piMat)

## Setup
df_dim1 <-
  tribble(
    ~rho, ~U, ~d, ~mu,
    1,  0,  0,   1,
    -3,  1,  1,  -1,
    0, -1,  0,  -1
  )

df_dim2 <-
  tribble(
    ~a, ~b,
    1,  0,
    0,  1,
  )

df_dim3 <-
  tribble(
    ~a, ~b,
     0,  0,
     0,  0,
  )

df_dim4 <-
  tribble(
    ~rho, ~U, ~d, ~mu, ~e,
       1,  0,  0,   1,  0,
      -3,  1,  1,  -1,  1,
       0, -1,  0,  -1,  0
  )

df_dim4_labeled <-
  tribble(
    ~dim, ~rho, ~U, ~d, ~mu, ~e,
     "M",    1,  0,  0,   1,  0,
     "L",   -3,  1,  1,  -1,  1,
     "T",    0, -1,  0,  -1,  0
  )

## Pull a couple projectors
V1 <- pi_projection(df_dim1) %>% select(-out_name) %>% as.matrix();
V4 <- pi_projection(df_dim4) %>% select(-out_name) %>% as.matrix();

## Units
test_that(
  "Projections have correct dimensionality",
  {
    expect_equal(
      pi_projection(df_dim1) %>% dim() %>% .[[1]],
      1
    )
    expect_equal(
      pi_projection(df_dim4) %>% dim() %>% .[[1]],
      2
    )
  }
)

test_that(
  "Projections are orthogonal",
  {
    expect_equal(
      V1 %*% t(V1),
      diag(1)
    )
    expect_equal(
      V4 %*% t(V4),
      diag(2)
    )
  }
)

test_that(
  "Rank edge cases throw appropriate errors",
  {
    expect_error(
      pi_projection(df_dim2),
      "pi-subspace is zero-dimensional"
    )
    expect_error(
      pi_projection(df_dim3),
      "pi-subspace is full-dimensional"
    )
  }
)

test_that(
  "Non-numeric columns properly ignored",
  {
    expect_equal(
      pi_projection(df_dim4),
      pi_projection(df_dim4_labeled)
    )
  }
)
