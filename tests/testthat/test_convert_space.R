context("Subspace conversion")
library(piMat)

## Setup
df_given1 <-
  tribble(
    ~out_name, ~W1, ~W2,
         "X1",   1,  -1,
         "X2",   1,   0,
         "X3",   0,   1
  )

df_given2 <-
  tribble(
    ~out_name, ~W1, ~W2,
         "X1",   1,   1,
         "X2",   1,   1,
         "X3",   1,   1 - 1e-9
  )

df_given3 <-
  tribble(
    ~out_name, ~W1, ~W2,
         "X1",   1,   1,
         "X2",   1,   1,
         "X3",   1,   1 - 1e-3
  )

df_target1 <-
  tribble(
   ~out_name, ~V1, ~V2,
        "X1",   1,   0,
        "X2",   1,   1,
        "X3",   0,   1
  )

df_target2 <-
  tribble(
   ~out_name, ~V1, ~V2,
        "X1",   1,   1 - 1e-9,
        "X2",   1,   1,
        "X3",   0,   0
  )

df_target3 <-
  tribble(
   ~out_name, ~V1, ~V2,
        "X1",   1,   1 - 1e-3,
        "X2",   1,   1,
        "X3",   0,   0
  )

df_noout <-
  tribble(
    ~W1, ~W2,
      1,  -1,
      1,   0,
      0,   1
 )

df_short <-
  tribble(
    ~out_name, ~W1, ~W2,
         "X1",   1,  -1,
         "X2",   1,   0
  )

## Conversion loop
df_V2W1 <- convert_space(df_given1, df_target1)

df_t1 <- matched_inner(df_target1, transpose(df_V2W1)) %>%
  mutate(out_name = df_target1 %>% names() %>% setdiff(., "out_name"))
df_given1_p <- transpose(df_t1)

## Units
test_that(
  "Full-loop conversion accurate",
  {
    expect_true(
      all(near(
        df_given1 %>% select(-out_name) %>% as.matrix(),
        df_given1_p %>% select(-out_name) %>% as.matrix()
      ))
    )
  }
)

test_that(
  "Invariants checked",
  {
    expect_error(
      convert_space(df_noout, df_target1),
      "df_given does not contain an out_name column"
    )
    expect_error(
      convert_space(df_given1, df_noout),
      "df_target does not contain an out_name column"
    )
    expect_error(
      convert_space(df_short, df_target1),
      "out_name columns in df_given and df_target do not match!"
    )
    expect_error(
      convert_space(df_given1, df_short),
      "out_name columns in df_given and df_target do not match!"
    )
  }
)

test_that(
  "Condition number issues caught",
  {
    expect_error(
      convert_space(df_given2, df_target1),
      "df_given not full rank; stopping."
    )
    expect_error(
      convert_space(df_given1, df_target2),
      "df_target not full rank; stopping."
    )
    expect_warning(
      convert_space(df_given3, df_target1),
      "df_given is ill-conditioned"
    )
    expect_warning(
      convert_space(df_given1, df_target3),
      "df_target is ill-conditioned"
    )
  }
)
