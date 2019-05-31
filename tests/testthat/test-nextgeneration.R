# function next_generation
test_that("stop messages occur correctly", {
  population <- create_population(10)
  all_scores  <- life_cycle(population = population, grid_size = c(10, 10),
                           n_evidence = 10, steps = 10, sessions = 5)
  expect_error(next_generation(population, all_scores = c(1, 2, 3)))
})

test_that("next_generation returns a list containing data.frames", {
  population <- create_population(100)
  all_scores  <- life_cycle(population = population, grid_size = c(10, 10),
                           n_evidence = 10, steps = 10, sessions = 5)
  model <- next_generation(population, all_scores)
  expect_is(model, "list")
  expect_is(model[[1]], "data.frame")
})
