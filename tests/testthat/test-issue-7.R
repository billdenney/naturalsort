stopifnot(require(testthat))
context("issue#7")

library(naturalsort)

test_that("Support natural numbers, integers, real numbers, and scientific notation", {
  expect_equal(naturalorder(c("2", "1", "0"), interpretation="natural"),
               c(3, 2, 1))
  expect_equal(naturalorder(c("0.10", "0.020", "0.0030"), interpretation="natural"),
               1:3)
  expect_equal(naturalorder(c("-0.10", "-0.010", "-0.0010"), interpretation="natural"),
               3:1,
               info="order maintains stable sorting")
  expect_equal(naturalorder(c("1E3", "1E+4", "1E+5"), interpretation="natural"),
               1:3)
  expect_equal(naturalorder(c("1.0.1", "1.1.0", "1.0"), interpretation="natural"),
               c(3, 1, 2))
  expect_equal(naturalorder(c("1.0-1", "1.1-0", "1.0", "1.0-0"), interpretation="natural"),
               c(3, 4, 1, 2))

  expect_equal(naturalorder(c("2", "1", "0"), interpretation="integer"),
               c(3, 2, 1))
  expect_equal(naturalorder(c("0.10", "0.020", "0.0030"), interpretation="integer"),
               1:3)
  expect_equal(naturalorder(c("-0.10", "-0.010", "-0.0010"), interpretation="integer"),
               3:1,
               info="order maintains stable sorting")
  expect_equal(naturalorder(c("1E3", "1E+4", "1E+5"), interpretation="integer"),
               1:3)
  expect_equal(naturalorder(c("1.0.1", "1.1.0", "1.0"), interpretation="integer"),
               c(3, 1, 2))
  expect_equal(naturalorder(c("1.0-1", "1.1-0", "1.0", "1.0-0"), interpretation="integer"),
               c(3, 1, 4, 2))

  expect_equal(naturalorder(c("2", "1", "0"), interpretation="real"),
               c(3, 2, 1))
  expect_equal(naturalorder(c("0.10", "0.020", "0.0030"), interpretation="real"),
               3:1)
  expect_equal(naturalorder(c("-0.10", "-0.010", "-0.0010"), interpretation="real"),
               1:3)
  expect_equal(naturalorder(c("1E3", "1E+4", "1E+5"), interpretation="real"),
               1:3)
  expect_equal(naturalorder(c("1.0.1", "1.1.0", "1.0"), interpretation="real"),
               c(3, 1, 2))
  expect_equal(naturalorder(c("1.0-1", "1.1-0", "1.0", "1.0-0"), interpretation="real"),
               c(3, 1, 4, 2))

  expect_equal(naturalorder(c("2", "1", "0"), interpretation="scientific"),
               c(3, 2, 1))
  expect_equal(naturalorder(c("0.10", "0.020", "0.0030"), interpretation="scientific"),
               3:1)
  expect_equal(naturalorder(c("-0.10", "-0.010", "-0.0010"), interpretation="scientific"),
               1:3)
  expect_equal(naturalorder(c("1E3", "1E+4", "1E+5"), interpretation="scientific"),
               1:3)
  expect_equal(naturalorder(c("1.0.1", "1.1.0", "1.0"), interpretation="scientific"),
               c(3, 1, 2))
  expect_equal(naturalorder(c("1.0-1", "1.1-0", "1.0", "1.0-0"), interpretation="scientific"),
               c(3, 1, 4, 2))
})
