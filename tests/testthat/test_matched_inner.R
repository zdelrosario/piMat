context("Inner product")
library(piMat)

## Setup
df_w1 <-
    tribble(
        ~out_name, ~X1, ~X2,
             "Y1",  +1,  -1
    )

df_w2 <-
    tribble(
        ~out_name, ~X1, ~X2,
             "Y1",  +1,  -1,
             "Y2",  +1,  +1
    )

df_w3 <-
    tribble(
        ~out_name, ~X2,
             "Y1",  +1
    )

df_w4 <-
    tribble(
        ~out_name, ~X1, ~X2, ~X3,
             "Y1",  +1,  -1,  +0,
             "Y2",  +1,  +1,  +1
    )

df_w5 <-
    tribble(
        ~X1, ~X2,
         +1,  -1,
         +1,  +1
    )

df_target <-
    tribble(
        ~X1, ~X2,
          1,   0,
          0,   1,
        0.5, 0.5
    )

df_target2 <-
    tribble(
        ~out_name, ~X1, ~X2,
             "V1",   1,   0,
             "V2",   0,   1,
             "V3", 0.5, 0.5
    )

## Units
test_that(
    "Output names match",
    {
        expect_equal(
            matched_inner(df_w1, df_target) %>% names(),
            df_w1 %>% pull(out_name)
        )
        expect_equal(
            matched_inner(df_w2, df_target) %>% names(),
            df_w2 %>% pull(out_name)
        )
    }
)

test_that(
    "Optional output rownames match",
    {
        expect_equal(
            matched_inner(df_w1, df_target2) %>% pull(out_name),
            df_target2 %>% pull(out_name)
        )
    }
)

test_that(
    "Correct columns selected",
    {
        expect_equal(
            matched_inner(df_w3, df_target) %>% pull(Y1),
            df_target %>% pull(X2)
        )
    }
)

test_that(
    "Invariants checked",
    {
        expect_error(
            matched_inner(df_w4, df_target),
            "df_weights contains variables not found in df_target"
        )
        expect_error(
            matched_inner(df_w5, df_target),
            "df_weights does not contain an out_name column"
        )
    }
)
