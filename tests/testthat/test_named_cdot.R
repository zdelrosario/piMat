context("Named cdot")
library(piMat)

## Setup
weights <- c("X1" = +0.5, "X2" = -0.5)

df_target <-
  tribble(
    ~X1, ~X2,
      4,   2,
      8,   6
  )

mat_expected <-
  matrix(
    c(
      2, -1,
      4, -3
    ),
    nrow = 2
  ) %>%
  t()
colnames(mat_expected) <- c("X1", "X2")

## Units
test_that(
    "Elementwise product (cdot)",
    {
        expect_equal(
            named_cdot(weights, df_target),
            mat_expected
        )
    }
)
