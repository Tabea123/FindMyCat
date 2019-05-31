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

