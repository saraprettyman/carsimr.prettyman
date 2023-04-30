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
    final_grid <- cpp_convert_carsimr(
      as.matrix(move_cars_cpp(initial_grid, t)[[t + 1]])
    )
    count <- sum(final_grid == 1 | final_grid == 2)
    actual_count[t] <- count
    expect_equal(count, expected_count)
  }

  # Check that the total count stays consistent
  expect_equal(sum(actual_count), expected_count * 10)
})
