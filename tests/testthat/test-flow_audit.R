df_base <- data.frame(
  p1 = c(1, 2, 1, 2, 1, 1, 2, 1, 2, 1),         # n=10
  p2 = c(1, NA, 1, NA, 1, 1, NA, 1, NA, 1),       # n=6
  p3 = c(NA, NA, 1, NA, 2, 1, NA, 2, NA, 1)        # n=4
)

test_that("flow_audit returns a data.frame with correct columns", {
  result <- flow_audit(df_base, c("p1", "p2", "p3"))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("variable", "parteaguas", "n_esperado",
                         "n_real", "pct_entrada", "alerta"))
})

test_that("first variable has Inicio as parteaguas and n_esperado = nrow", {
  result <- flow_audit(df_base, c("p1", "p2", "p3"))
  expect_equal(result$parteaguas[1], "(Inicio)")
  expect_equal(result$n_esperado[1], nrow(df_base))
  expect_equal(result$n_real[1], 10L)
})

test_that("n_real counts non-NA correctly", {
  result <- flow_audit(df_base, c("p1", "p2", "p3"))
  expect_equal(result$n_real, c(10L, 6L, 5L))
})

test_that("parteaguas persists when n stays stable after a drop", {
  result <- flow_audit(df_base, c("p1", "p2", "p3"))
  # p2 drops below threshold -> p2 becomes parteaguas for p3
  expect_equal(result$parteaguas[3], "p2")
})

test_that("alert is OK when no drop occurs", {
  df_full <- data.frame(p1 = 1:10, p2 = 1:10)
  result <- flow_audit(df_full, c("p1", "p2"))
  expect_equal(result$alerta[2], "OK")
})

test_that("stops with informative error for missing variables", {
  expect_error(flow_audit(df_base, c("p1", "p99")), "p99")
})

test_that("stops if data is not a data.frame", {
  expect_error(flow_audit(list(a = 1), "a"), "data.frame")
})
