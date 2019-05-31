# function lookup_situation
test_that("stop messages occur correctly", {
  grid <- create_grid(c(5, 5), 5)
  expect_error(lookup_situation(individual, grid, latitude = 2, longitude = 3))
  individual <- create_population(10)[[10]]
  expect_error(lookup_situation(individual, grid, latitude = 7, longitude = 3))
  expect_error(lookup_situation(individual, grid, latitude = c(7, 8, 7, 9, 7), longitude = 3))
  grid <- data.frame(matrix("Empty", ncol = 3, nrow = 3))
  expect_error(lookup_situation(individual, grid, latitude = 2, longitude = 3))
})

test_that("lookup_situation returns a number", {
  grid <- create_grid(c(5, 5), 5)
  individual <- create_population(10)[[10]]
  model <- lookup_situation(individual, grid, latitude = 4, longitude = 3)
  expect_is(model, "numeric")
})


