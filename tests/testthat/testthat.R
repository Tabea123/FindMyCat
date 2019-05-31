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
  grid <- data.frame(matrix("Empty", ncol = 3, nrow = 3))
  expect_error(lookup_situation(individual, grid, latitude = 2, longitude = 3))
})

test_that("lookup_situation returns a number", {
  grid <- create_grid(c(5, 5), 5)
  individual <- create_population(10)[[10]]
  model <- lookup_situation(individual, grid, latitude = 4, longitude = 3)
  expect_is(model, "numeric")
})


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
  expect_less_than(move_score(individual, grid, steps = 200)$score, 51)
})

test_that("move_score returns a numeric score", {
  model <- move_score(individual, grid, steps = 200)$score
  expect_is(model, "numeric")
})

test_that("move_score returns a coordinates within the boundaries of the grid", {
  grid <- create_grid(c(5, 7), 5)
  model <- move_score(individual, grid, steps = 200)
  expect_less_than(model$latitude, 7)
  expect_more_than(model$latitude, 1)
  expect_less_than(model$longitude, 9)
  expect_more_than(model$longitude, 1)
})

test_that("score cannot be over max score", {
  grid <- create_grid(c(5, 5), 5)
  individual <- create_population(10)[[10]]
  expect_less_than(move_score(individual, grid, steps = 200)$score, 50)
})

# function life
test_that("stop messages occur correctly", {
  population <- create_population(10)
  expect_error(life_cycle(population, grid_size = "a", n_evidence = 10, steps = 3,
                    sessions = 2))
  expect_error(life_cycle(population, grid_size = c(10, 5), n_evidence = 10, steps = 3,
                    sessions = "three"))
  expect_error(life_cycle(population = population, grid_size = c(10, 10), steps = 100,
                          sessions = 1))
})

test_that("life returns a matrix of numbers", {
  population <- create_population(10)
  model <- life_cycle(population, grid_size = c(5, 5), n_evidence = 11, steps = 3,
                sessions = 3)
  expect_is(model, "matrix")
  expect_is(model[1,1], "numeric")
})

test_that("score cannot be over the maximum possibe score of n_evidence * 10", {
  population <- create_population(10)
  expect_lt(sum(life_cycle(population, grid_size = c(5, 5), n_evidence = 11,
                            steps = 3, sessions = 3) > 5*11), 1)
})

# function next_generation
test_that("stop messages occur correctly", {
  population <- create_population(10)
  expect_error(next_generation(population, all_scores = c(1, 2, 3)))
})

test_that("next_generation returns a list containing data.frames", {
  population <- create_population(100)
  all_score  <- life_cycle(population = population, grid_size = c(10, 10),
                           n_evidence = 10, steps = 10, sessions = 5)
  model <- next_generation(population, all_scores)
  expect_is(model, "list")
  expect_is(model[[1]], "data.frame")
})

# function evolution
test_that("stop messages occur correctly", {
  expect_error(evolution(population_size = 100, grid_size = c(5, 5), n_evidence = 5,
                        steps = 30, sessions = 40, generations = "10"))
})

test_that("next_generation returns a data.frame", {
  model <- evolution(100, c(10, 10), 11, 5, 3, 2)
  expect_is(model, "data.frame")
  expect_equal(nrow(model), 243)
  expect_equal(ncol(model), 6)
})

# function visualize_path
test_that("stop messages occur correctly", {
  population1 <- create_population(10)
  expect_error(visualize_path(
    individual = population1[[3]], grid_size = c(5, 7), evidence_latitude =
      c(3, 2), evidence_longitude = c(1, 6, 5), steps = 30, animal = "bird"))
  expect_error(visualize_path(
    individual = population1[[3]], grid_size = c(5, 7), evidence_latitude =
      c(3, 2, 2), evidence_longitude = c(1, 6, 5), steps = 30, animal = bird))
})
