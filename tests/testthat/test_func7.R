
context("homework 1 function 7 ")

test_that("the result of example works correctly", {
    x <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
    func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
    interval <- c(0,100)
    expect_true(abs(func7(x,func1,interval)-2.962418) < 4.27e-07)
})
