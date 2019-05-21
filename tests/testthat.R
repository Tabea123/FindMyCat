library(testthat)
library(findmycat)

test_check("findmycat")

# function create_population
test_that("the argument individuals works properly", {
  expect_equal(length(create_population(200)), 200)
  expect_equal(length(create_population(10)), 10)
})

test_that("stop messages occur correctly", {
  expect_error(create_population("200"))
  expect_error(create_population(NA))
})

test_that("first move is never stay", {
  for(i in 1:100){
  expect_false(create_population(100)[[i]]$Move[1] == "Stay")
}
})

# function create_grid
test_that("stop messages occur", {
  expect_error(create_grid(coordinates_grid = c(1, b), n_evidence = 11))

  expect_error(create_grid(coordinates_grid = c(1, 9, 1), n_evidence = 11))

  expect_error(create_grid(coordinates_grid = c(9, 9), n_evidence = "footprints"))
})

test_that("create_grid creates a matrix", {
  expect_equal(class(grid), "matrix")
})

test_that("create_grid creates a wall at the boarders", {
  for(i in 1:12){
  expect_true(create_grid(coordinates_grid = c(10, 10), n_evidence = 11)[i,1] == "Wall")
  expect_true(create_grid(coordinates_grid = c(10, 10), n_evidence = 11)[i,12] == "Wall")
  expect_true(create_grid(coordinates_grid = c(10, 10), n_evidence = 11)[1,i] == "Wall")
  expect_true(create_grid(coordinates_grid = c(10, 10), n_evidence = 11)[12,i] == "Wall")
  }
})

test_that("create_grid creates as much evidence as the user indicates", {
  expect_equal(sum(create_grid(coordinates_grid = c(10, 10), n_evidence = 11)
                   == "Evidence"), 11)
})
