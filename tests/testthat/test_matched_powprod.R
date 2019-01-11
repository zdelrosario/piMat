context("Power-product")
library(piMat)

## Setup
df_weights <-
    tribble(
        ~out_name, ~X1, ~X2,
             "Y1",  +1,  -1
    )

df_target <-
    tribble(
        ~X1, ~X2,
          2,   1,
          1,   2
    )

df_expected <-
  tibble(
    Y1 = c(2, 0.5)
  )

## Units
test_that(
    "Products and ratios",
    {
        expect_equal(
            matched_powprod(df_weights, df_target),
            df_expected
        )
    }
)
