library(testthat)
library(findmycat)

test_check("findmycat")

# function create_grid
test_that("stop messages occur", {
  expect_error(create_grid(grid_size = c(1, b), n_evidence = 11))

  expect_error(create_grid(grid_size = c(1, 9, 1), n_evidence = 11))

  expect_error(create_grid(grid_size = c(9, 9), n_evidence = "footprints"))
})

test_that("there can not be more evidence than grid fields", {
  expect_error(create_grid(grid_size = c(1, 1), n_evidence = 11))
})

test_that("create_grid creates a matrix", {
  model <- create_grid(grid_size = c(9, 9), n_evidence = 11)
  expect_is(model, "matrix")
})

test_that("create_grid creates a wall at the boarders", {
  for(i in 1:12){
  expect_true(create_grid(grid_size = c(10, 10), n_evidence = 11)[i,1] == "Wall")
  expect_true(create_grid(grid_size = c(10, 10), n_evidence = 11)[i,12] == "Wall")
  expect_true(create_grid(grid_size = c(10, 10), n_evidence = 11)[1,i] == "Wall")
  expect_true(create_grid(grid_size = c(10, 10), n_evidence = 11)[12,i] == "Wall")
  }
})

test_that("create_grid creates as much evidence as the user indicates", {
  expect_equal(sum(create_grid(grid_size = c(10, 10), n_evidence = 11)
                   == "Evidence"), 11)
})

# function create_population
test_that("the argument individuals works properly", {
  expect_equal(length(create_population(200)), 200)
  expect_equal(length(create_population(10)), 10)
})

test_that("stop messages occur correctly", {
  expect_error(create_population("200"))
  expect_error(create_population(NA))
})

test_that("create_population creates a list containing data.frames", {
  model <- create_population(100)
  expect_is(model, "list")
  expect_is(model[[1]], "data.frame")
})

# function lookup_situation
test_that("stop messages occur correctly", {
  grid <- create_grid(c(5, 5), 5)
  expect_error(lookup_situation(individual, grid, latitude = 2, longitude = 3))
  individual <- create_population(10)[[10]]
  expect_error(lookup_situation(individual, grid, latitude = 7, longitude = 3))
  expect_error(lookup_situation(individual, grid, latitude = c(7, 8, 7, 9, 7), longitude = 3))
})


