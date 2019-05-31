# function move_score
test_that("stop messages occur correctly", {
  grid <- create_grid(c(5, 7), 5)
  population <- create_population(10)

  expect_error(move_score(population, grid, latitude = 2, longitude = 3,
                          steps = 10))
  expect_error(move_score(population[[10]], grid, latitude = c(7, 8, 7, 9, 7),
                          longitude = 3, steps = 10))
  expect_error(move_score(population[[1]], grid, latitude = 2, longitude = 3,
                          steps = "10"))

  grid <- data.frame(matrix("Empty", ncol = 3, nrow = 3))
  expect_error(move_score(individual, grid, latitude = 2, longitude = 3, steps = 10))
})

test_that("score cannot be over the maximum possibe score of n_evidence * 10", {
  grid <- create_grid(c(5, 5), 5)
  individual <- create_population(10)[[10]]
  expect_lt(move_score(individual, grid, steps = 200)$score, 51)
})

test_that("move_score returns a numeric score", {
  grid <- create_grid(c(5, 7), 5)
  individual <- create_population(10)[[10]]
  model <- move_score(individual, grid, steps = 200)$score
  expect_is(model, "numeric")
})

test_that("move_score returns a coordinates within the boundaries of the grid", {
  grid <- create_grid(c(5, 7), 5)
  individual <- create_population(10)[[10]]
  model <- move_score(individual, grid, steps = 200)
  expect_lt(model$latitude, 7)
  expect_gt(model$latitude, 1)
  expect_lt(model$longitude, 9)
  expect_gt(model$longitude, 1)
})

test_that("score cannot be over max score", {
  grid <- create_grid(c(5, 5), 5)
  individual <- create_population(10)[[10]]
  expect_lt(move_score(individual, grid, steps = 200)$score, 50)
})

