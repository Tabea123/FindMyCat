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
