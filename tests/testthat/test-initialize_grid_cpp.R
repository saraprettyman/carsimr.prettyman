test_that("No red cars when prob_blue=1", {
  rho <- 0.50
  prob_blue <- 1
  dims <- c(10, 10)
  test_grid <- initialize_grid_cpp(rho, dims, prob_blue)
  actual_count <- sum(test_grid == 2)

  expect_equal(actual_count, 0)
})

test_that("No cars when rho = 0", {
  rho <- 0
  prob_blue <- 0.50
  dims <- c(10, 10)
  test_grid <- initialize_grid_cpp(rho, dims, prob_blue)
  actual_count <- sum(test_grid == 2 | test_grid == 1)

  expect_equal(actual_count, 0)
})

test_that("No blank spaces when rho = 0.999", {
  rho <- 0.999
  prob_blue <- 0.50
  dims <- c(3, 3)
  test_grid <- initialize_grid_cpp(rho, dims, prob_blue)
  fill_count <- sum(test_grid == 2 | test_grid == 1)
  fill_count

  expect_true(ceiling(dims[1] * dims[2] * rho) == fill_count)
})

test_that("Non-integer number returns proper car count", {
  rho <- runif(1)
  prob_blue <- 0.50
  dims <- c(5, 5)
  num_cars <- rho * dims[1] * dims[2]
  expected_count1 <- ceiling(num_cars)
  expected_count2 <- trunc(num_cars)

  test_grid <- initialize_grid_cpp(rho, dims, prob_blue)
  actual_count <- sum(test_grid == 2 | test_grid == 1)

  test <- actual_count == expected_count1 || actual_count == expected_count2
  expect_true(test)
})
