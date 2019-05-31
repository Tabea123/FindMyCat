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

