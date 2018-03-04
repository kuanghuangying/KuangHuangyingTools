context("Homework functions")

test_that("func1 computes mean, var, sd", {
         x <- 1:10
         var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
         x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
         expect_identical(func1(x), x_list)
         })

test_that("func2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func2(x), x_list)
  save<-try(func2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})


test_that("func3 computes mle", {
  x <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
  val <- func3(x)
  expect_true(abs(val-2.962401) < 4.27e-07)
})


test_that("func4 computes the weighted mean, var, sd", {
  val <- func4(d)
  expect <- list(mean=mean(d),var=var(d),sd=sd(d))
  expect_identical(val,expect)
})

test_that("func5 computes the weighted mean, var, sd", {
  val <- func5(d)
  expect <- list(mean=mean(d),var=var(d),sd=sd(d))
  expect_identical(val,expect)
})

expect_message(func6(NA), "not numeric")

test_that("func6 catches error", {
  expect_message(func6(NA), "not numeric")
})


test_that("hw2_1 calculates xTA−1x", {
  A <- matrix( c(5, 1, 0,
                 3,-1, 2,
                 4, 0,-1), nrow=3, byrow=TRUE)
  A_inv = matrix( c(0.0625, 0.0625, 0.125,
                    0.6875,-0.3125, -0.625,
                    0.2500, 0.250,-0.500), nrow=3, byrow=TRUE)
  x = c(7,8,9)
  x <- matrix(x,ncol=1)
  val <-hw2_1(A,x)
  expect = t(x)%*%A_inv%*%x
  expect_identical(val,expect)
})


test_that("%question2%", {
  A <- matrix( c(5, 1, 0,
                 3,-1, 2,
                 4, 0,-1), nrow=3, byrow=TRUE)
  A_inv = matrix( c(0.0625, 0.0625, 0.125,
                    0.6875,-0.3125, -0.625,
                    0.2500, 0.250,-0.500), nrow=3, byrow=TRUE)
  x = c(7,8,9)
  x <- matrix(x,ncol=1)
  val <-A %question2% x
  expect = t(x)%*%A_inv%*%x
  expect_identical(val,expect)
})


test_that("q3 computes standardized matrix", {
  val <- q3(matrix(rnorm(20),nc=5,nr=4))
  expect <- scale(matrix(rnorm(20),nc=5,nr=4))
  expect_identical(val,expect)
})

test_that("q4 computes standardized matrix", {
  val <- q4(matrix(rnorm(20),nc=5,nr=4))
  expect <- scale(matrix(rnorm(20),nc=5,nr=4))
  expect_identical(val,expect)
})


test_that("myapply works as apply", {
  val <- myapply(matrix(1:6, nrow = 3, ncol = 2),1,mean)
  expect <- apply(matrix(1:6, nrow = 3, ncol = 2),1,mean)
  expect_identical(val,expect)
})
