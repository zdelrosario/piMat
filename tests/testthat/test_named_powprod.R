context("Named Power-product")
library(piMat)

## Setup
df_weights <-
  c("X1" = +1, "X2" = -1)

df_target <-
    tribble(
        ~X1, ~X2,
          2,   1,
          1,   2
    )

expected <-
  c(2, 0.5)

## Units
test_that(
    "Products and ratios",
    {
        expect_equal(
            named_powprod(df_weights, df_target),
            expected
        )
    }
)
