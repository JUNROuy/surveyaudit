df_audit <- data.frame(
  p1 = c(1,2,1,2,1,1,2,1,2,1),
  p2 = c(1,NA,1,NA,1,1,NA,1,NA,1),
  p3 = c(NA,NA,1,NA,2,1,NA,2,NA,1)
)

audit <- flow_audit(df_audit, c("p1","p2","p3"))

test_that("plot_flow_sankey returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  p <- plot_flow_sankey(audit)
  expect_s3_class(p, "ggplot")
})

test_that("plot_flow_tree returns a ggplot object", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("igraph")
  p <- plot_flow_tree(audit)
  expect_s3_class(p, "ggplot")
})

test_that("plot_flow_sankey errors on wrong input", {
  expect_error(plot_flow_sankey(data.frame(x = 1)), "flow_audit")
})

test_that("plot_flow_tree errors on wrong input", {
  skip_if_not_installed("ggraph")
  expect_error(plot_flow_tree(data.frame(x = 1)), "flow_audit")
})

test_that("plot_flow_sankey accepts custom title", {
  skip_if_not_installed("ggplot2")
  p <- plot_flow_sankey(audit, title = "Mi titulo")
  expect_equal(p$labels$title, "Mi titulo")
})
