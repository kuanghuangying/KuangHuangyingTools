
context("plot data ")

test_that("plot data is plotted", {
    p <- plotMyData()
    expect_true(is.ggplot(p))

  #expect_identical(fbind(x, y), z)
  #expect_identical(fbind(x_fact, y), z)
})
