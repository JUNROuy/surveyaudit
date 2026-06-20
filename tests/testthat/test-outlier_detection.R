set.seed(42)

# ── detect_hb ──────────────────────────────────────────────────────────────

test_that("detect_hb returns correct structure", {
  yt1 <- c(100, 200, 150, 300, 250, 180, 220, 170, 190, 210)
  yt2 <- c(105, 210, 148, 315, 245, 185, 225, 168, 195, 212)
  res <- detect_hb(yt1, yt2)
  expect_type(res, "list")
  expect_named(res, c("outlier", "score", "limits", "method"))
  expect_length(res$outlier, length(yt1))
  expect_length(res$score,   length(yt1))
  expect_named(res$limits, c("lower", "upper"))
})

test_that("detect_hb flags an extreme outlier", {
  yt1 <- c(rep(100, 9), 100)
  yt2 <- c(rep(105, 9), 999999)
  res <- detect_hb(yt1, yt2)
  expect_true(res$outlier[10])
  expect_false(any(res$outlier[1:9], na.rm = TRUE))
})

test_that("detect_hb errors on unequal lengths", {
  expect_error(detect_hb(1:5, 1:4), "same length")
})

test_that("detect_hb errors on non-numeric input", {
  expect_error(detect_hb(letters[1:5], 1:5), "numeric")
})

test_that("detect_hb errors on invalid U", {
  expect_error(detect_hb(1:5, 1:5, U = 1.5), "between 0 and 1")
})

test_that("detect_hb handles NAs without crashing", {
  yt1 <- c(100, NA, 150, 200, 250, 180, 220, 170, 190, 210)
  yt2 <- c(105, 210, NA, 215, 245, 185, 225, 168, 195, 212)
  res <- detect_hb(yt1, yt2)
  expect_true(is.na(res$outlier[2]))
  expect_true(is.na(res$outlier[3]))
})

# ── detect_sabp ────────────────────────────────────────────────────────────

test_that("detect_sabp returns correct structure", {
  x   <- c(rlnorm(48, meanlog = 5), 1e9, 2e9)
  res <- detect_sabp(x)
  expect_type(res, "list")
  expect_named(res, c("outlier", "score", "limits", "medcouple", "method"))
  expect_length(res$outlier, length(x))
  expect_true(res$limits["upper"] > res$limits["lower"])
})

test_that("detect_sabp detects extreme right-tail outliers", {
  x   <- c(rnorm(50, 100, 5), 1e6, 2e6)
  res <- detect_sabp(x)
  expect_true(res$outlier[51])
  expect_true(res$outlier[52])
})

test_that("detect_sabp score is NA for non-outliers", {
  x   <- rnorm(30, 100, 5)
  res <- detect_sabp(x)
  expect_true(all(is.na(res$score[!res$outlier & !is.na(res$outlier)])))
})

test_that("detect_sabp handles NAs", {
  x   <- c(rnorm(10), NA, NA)
  res <- detect_sabp(x)
  expect_true(is.na(res$outlier[11]))
  expect_true(is.na(res$outlier[12]))
})

test_that("detect_sabp errors on non-numeric input", {
  expect_error(detect_sabp(letters[1:10]), "numeric")
})

# ── outlier_summary ─────────────────────────────────────────────────────────

test_that("outlier_summary returns a data.frame with expected columns", {
  df  <- data.frame(x = c(rnorm(48), 1e6, 2e6), y = c(rnorm(48), -1e6, 2e6))
  res <- outlier_summary(df, c("x", "y"), method = "sabp")
  expect_s3_class(res, "data.frame")
  expect_true(all(c("variable", "n_outliers", "pct_outliers") %in% names(res)))
  expect_equal(nrow(res), 2L)
})
