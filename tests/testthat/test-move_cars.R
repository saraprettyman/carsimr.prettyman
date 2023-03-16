# Load the test data
test_data <- readRDS(testthat::test_path("data/test_carsimr_data.RDS"))

test_that("Test Data Set 1", {
  # Subset the three test matrices
  test_set <- test_data[1]

  # Move the blue cars, then red cars of the initial test set
  initial_grid <- test_set[[1]][[1]]

  move_result <- move_blue(initial_grid)

  move_result2 <- move_red(move_result)

  # subset expect results and add class

  test_set1 <- test_set[[1]][[2]]

  test_set2 <- test_set[[1]][[3]]

  test_set1 <- carsimr(test_set1)

  test_set2 <- carsimr(test_set2)

  # Compare the first then second move to expect results

  expect_equal(move_result, test_set1)

  expect_equal(move_result2, test_set2)
})

test_that("Test Data Set 2", {
  # Subset the three test matrices
  test_set <- test_data[2]

  # Move the blue cars, then red cars of the initial test set
  initial_grid <- test_set[[1]][[1]]

  move_result <- move_blue(initial_grid)

  move_result2 <- move_red(move_result)

  # subset expect results and add class

  test_set1 <- test_set[[1]][[2]]

  test_set2 <- test_set[[1]][[3]]

  test_set1 <- carsimr(test_set1)

  test_set2 <- carsimr(test_set2)

  # Compare the first then second move to expect results

  expect_equal(move_result, test_set1)

  expect_equal(move_result2, test_set2)
})

test_that("Test Data Set 3", {
  # Subset the three test matrices
  test_set <- test_data[3]

  # Move the blue cars, then red cars of the initial test set
  initial_grid <- test_set[[1]][[1]]

  move_result <- move_blue(initial_grid)

  move_result2 <- move_red(move_result)

  # subset expect results and add class

  test_set1 <- test_set[[1]][[2]]

  test_set2 <- test_set[[1]][[3]]

  test_set1 <- carsimr(test_set1)

  test_set2 <- carsimr(test_set2)

  # Compare the first then second move to expect results

  expect_equal(move_result, test_set1)

  expect_equal(move_result2, test_set2)
})

test_that("Test Data Set 4", {
  # Subset the three test matrices
  test_set <- test_data[4]

  # Move the blue cars, then red cars of the initial test set
  initial_grid <- test_set[[1]][[1]]

  move_result <- move_blue(initial_grid)

  move_result2 <- move_red(move_result)

  # subset expect results and add class

  test_set1 <- test_set[[1]][[2]]

  test_set2 <- test_set[[1]][[3]]

  test_set1 <- carsimr(test_set1)

  test_set2 <- carsimr(test_set2)

  # Compare the first then second move to expect results

  expect_equal(move_result, test_set1)

  expect_equal(move_result2, test_set2)
})

test_that("Total number of cars stays consistent", {
  # initialize variables
  rho <- 0.50
  prob_blue <- 0.50
  dims <- c(10, 10)
  initial_grid <- initialize_grid(rho, dims, prob_blue)
  expected_count <- sum(initial_grid == 1 | initial_grid == 2)
  actual_count <- numeric(10)

  # Run simulation and count actual cars at each time step
  for (t in 1:10) {
    final_grid <- as.matrix(move_cars(initial_grid, t)[[t + 1]])
    count <- sum(final_grid == 1 | final_grid == 2)
    actual_count[t] <- count
    expect_equal(count, expected_count)
  }

  # Check that the total count stays consistent
  expect_equal(sum(actual_count), expected_count * 10)
})
