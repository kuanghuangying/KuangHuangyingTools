
context("select data ")

test_that("select data with three column names", {
    slicedData <- selectData()
    ncol <- ncol(slicedData)
    expect_true(ncol==3)
    expect_true("Petal.Length" %in% names(slicedData))
  expect_true("Petal.Width" %in% names(slicedData))
  expect_true("Species" %in% names(slicedData))
})
