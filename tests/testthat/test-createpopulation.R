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
