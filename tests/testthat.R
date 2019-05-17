library(testthat)
library(findmycat)

test_check("findmycat")

# function create_population
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
  expect_error(create_population(NA, situations = NA, moves = NA))
  expect_error(create_population(200, situations = "situations", moves = NA))
})

# function create_grid
test_that("stop messages occur correctly", {
  expect_error(create_grid(coordinates_grid = c(1, b, 1, c),
                           evidence_latitude = c(1, 2, 3),
                           evidence_longitude = c(1, 2, 3)))

  expect_error(create_grid(coordinates_grid = c(1, 9, 1, 9),
                           evidence_latitude = c(1, b, 3),
                           evidence_longitude = c(1, 2, 3)))

  expect_error(create_grid(coordinates_grid = c(1, 9, 1),
                           evidence_latitude = c(1, 2, 3),
                           evidence_longitude = c(1, 2, 3)))

  expect_error(create_grid(coordinates_grid = c(1, 9, 1, 9),
                           evidence_latitude = c(1, 3),
                           evidence_longitude = c(1, 2, 3)))
})

test_that("evidence outside the boarders gives a warning",{
  expect_warning(create_grid(coordinates_grid = c(0, 10, 0, 10),
                             evidence_latitude = c(0, 10, 3, 5, 8),
                             evidence_longitude = c(9, 4, 5, 7, 1)),
                 "Evidence outside the grid will not be considered")
})

test_that("create_grid creates a data.frame", {
  expect_equal(class(create_grid(coordinates_grid = c(0, 10, 0, 10),
                evidence_latitude = c(1, 2, 3),
                evidence_longitude = c(1, 2, 3))), "data.frame")
})
