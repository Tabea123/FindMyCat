library(testthat)
library(findmycat)

test_check("findmycat")

test_that("create_population creates a list", {
  expect_equal(class(
    create_population(200, situations = situations,
                      moves = c("North", "East", "South", "West", "Stay", "Pick-Up"))),
    "list")
})

test_that("the argument individuals works properly", {
  expect_equal(length(
    create_population(200, situations = situations,
                      moves = c("North", "East", "South", "West", "Stay", "Pick-Up"))),
    200)
  expect_equal(length(
    create_population(100, situations = situations,
                      moves = c("North", "East", "South", "West", "Stay", "Pick-Up"))),
    100)
})

test_that("stop messages occur correctly", {
  expect_error(create_population("200", situations = situations,
                                 moves = c("North", "East", "South",
                                           "West", "Stay", "Pick-Up")))
  expect_error(create_population(200, situations = situations, moves = 5))
  expect_error(create_population(NA, situations = situations, moves = NA))
})
