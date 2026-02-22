library(testthat)
library(afrobarometer.cleaning)

test_that("ab_replace_sentinels replaces all sentinel values with NA", {
  df <- data.frame(x = c(-1, 0, 7, 8, 9, 3, 99), y = c(1, 2, 3, 4, 5, 6, 7))
  result <- ab_replace_sentinels(df)
  expect_true(is.na(result$x[1]))   # -1
  expect_false(is.na(result$x[2]))  # 0 – valid
  expect_true(is.na(result$x[3]))   # 7
  expect_true(is.na(result$x[4]))   # 8
  expect_true(is.na(result$x[5]))   # 9
  expect_false(is.na(result$x[6]))  # 3 – valid
  expect_true(is.na(result$x[7]))   # 99
})

test_that("ab_replace_sentinels respects the cols argument", {
  df <- data.frame(a = c(-1, 2), b = c(-1, 2))
  result <- ab_replace_sentinels(df, cols = "a")
  expect_true(is.na(result$a[1]))
  expect_false(is.na(result$b[1]))  # b not cleaned
})

test_that("ab_binarize works with explicit threshold", {
  df <- data.frame(score = c(1, 2, 3, 4, NA))
  result <- ab_binarize(df, cols = "score", threshold = 2)
  expect_equal(result$score, c(0L, 0L, 1L, 1L, NA))
})

test_that("ab_binarize preserves NA", {
  df <- data.frame(x = c(1, NA, 3))
  result <- ab_binarize(df, "x", threshold = 2)
  expect_true(is.na(result$x[2]))
})

test_that("ab_binarize warns for missing column", {
  df <- data.frame(a = 1:3)
  expect_warning(ab_binarize(df, cols = "nonexistent", threshold = 1))
})

test_that("ab_recode_education returns ordered factor", {
  x <- c(0, 2, 5, 7, 9, NA)
  result <- ab_recode_education(x)
  expect_s3_class(result, "ordered")
  expect_equal(as.character(result[1]), "No schooling")
  expect_equal(as.character(result[2]), "Informal or primary")
  expect_equal(as.character(result[3]), "Secondary")
  expect_equal(as.character(result[4]), "University or tech")
  expect_equal(as.character(result[5]), "Advanced")
  expect_true(is.na(result[6]))
})

test_that("ab_scale_item scales 0-4 to 0-1 and NAs out-of-range", {
  x <- c(0, 1, 2, 3, 4, -1, 5, NA)
  result <- ab_scale_item(x)
  expect_equal(result[1], 0)
  expect_equal(result[4], 0.75)
  expect_equal(result[5], 1.0)
  expect_true(is.na(result[6]))   # -1
  expect_true(is.na(result[7]))   # 5
  expect_true(is.na(result[8]))   # NA passthrough
})

test_that("ab_lookup_var returns correct mapping", {
  expect_equal(ab_lookup_var("news_radio", round = 5), "Q13A")
  expect_equal(ab_lookup_var("news_radio", round = 9), "Q74A")
  expect_true(is.na(ab_lookup_var("news_sm", round = 5)))  # not in R5
})

test_that("ab_lookup_var errors on unknown concept", {
  expect_error(ab_lookup_var("not_a_real_var", round = 5))
})

test_that("ab_clean_round works on synthetic data", {
  # Build a minimal synthetic data frame mimicking round 5 structure
  n <- 50
  set.seed(42)
  fake_r5 <- data.frame(
    RESPNO = 1:n,
    REGION = sample(1:5, n, replace = TRUE),
    URBRUR = sample(1:2, n, replace = TRUE),
    Q97    = sample(c(0:9, -1, 9), n, replace = TRUE),
    Q15    = sample(c(0:3, -1, 9), n, replace = TRUE),
    Q13A   = sample(0:4, n, replace = TRUE),
    Q13B   = sample(0:4, n, replace = TRUE),
    Q13C   = sample(0:4, n, replace = TRUE),
    Q13D   = sample(0:4, n, replace = TRUE),
    Q26B   = sample(0:5, n, replace = TRUE),
    Q26D   = sample(0:5, n, replace = TRUE),
    Q27    = sample(c(0:8, -1), n, replace = TRUE),
    Q29A   = sample(0:1,  n, replace = TRUE),
    Q29B   = sample(0:1,  n, replace = TRUE),
    Q29C   = sample(0:1,  n, replace = TRUE),
    EA_SVC_A = sample(0:1, n, replace = TRUE),
    EA_SVC_B = sample(0:1, n, replace = TRUE),
    EA_SVC_C = sample(0:1, n, replace = TRUE),
    EA_SVC_D = sample(0:1, n, replace = TRUE),
    EA_FAC_B = sample(0:1, n, replace = TRUE),
    EA_FAC_C = sample(0:1, n, replace = TRUE),
    EA_FAC_D = sample(0:1, n, replace = TRUE),
    EA_SEC_A = sample(0:1, n, replace = TRUE),
    EA_SEC_B = sample(0:1, n, replace = TRUE),
    EA_SEC_C = sample(0:1, n, replace = TRUE),
    EA_ROAD  = sample(0:1, n, replace = TRUE),
    Q30A = sample(0:3, n, replace = TRUE),
    Q30B = sample(0:3, n, replace = TRUE),
    Q30C = sample(0:3, n, replace = TRUE),
    Q61A = sample(c(0:4, 7), n, replace = TRUE),
    Q61C = sample(c(0:4, 7), n, replace = TRUE),
    Q61D = sample(c(0:4, 7), n, replace = TRUE),
    Q61E = sample(c(0:4, 7), n, replace = TRUE),
    Q66A = sample(0:4, n, replace = TRUE),
    Q67A = sample(0:4, n, replace = TRUE),
    Q67C = sample(0:4, n, replace = TRUE),
    Q67D = sample(0:4, n, replace = TRUE),
    Q67E = sample(0:4, n, replace = TRUE),
    Q91B = sample(0:4, n, replace = TRUE)
  )

  result <- ab_clean_round(fake_r5, round = 5,
                            binarize_vars = "local_gvt_performance")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), n)
  expect_true("civic_participation" %in% names(result))
  expect_true("news_weekly" %in% names(result))
  # binarized column should only be 0, 1, or NA
  expect_true(all(result$local_gvt_performance %in% c(0L, 1L, NA)))
})
