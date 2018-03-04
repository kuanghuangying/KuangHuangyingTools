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
