test_that("Move blue and move red have same total cars after", {
  # initialize variables
  rho <- 0.50
  dims <- c(5, 5)
  prob_blue <- 0.20
  test_grid <- initialize_grid(rho, dims, prob_blue)

  # move

  test_grid_blue <- move_blue(test_grid)
  test_grid_red <- move_red(test_grid)

  # calculate count

  test_grid_blue_total <- sum(test_grid_blue == 1) + sum(test_grid_blue == 2)
  test_grid_red_total <- sum(test_grid_red == 1) + sum(test_grid_red == 2)
  expected_total <- ceiling(rho * prod(dims))

  # test if counts equal

  expression <- (test_grid_blue_total & test_grid_red_total == expected_total)

  expect_true(expression)
})

test_that("Move blue car multiple times has same total cars", {
  # initialize variables
  rho <- 0.50
  dims <- c(5, 5)
  prob_blue <- runif(1)
  grid <- initialize_grid(rho, dims, prob_blue)
  actual_count <- numeric(100)
  expected_count <- sum(grid = 1)

  # test each individual as well as total count are equivalent
  for (t in 1:100) {
    test_grid_blue <- move_blue(grid)
    count <- sum(test_grid_blue = 1)
    actual_count[t] <- count
    expect_equal(count, expected_count)
  }

  expect_equal(sum(actual_count), expected_count * 100)
})

test_that("Move red car multiple times has same total cars", {
  # initialize variables
  rho <- 0.50
  dims <- c(5, 5)
  prob_blue <- runif(1)
  grid <- initialize_grid(rho, dims, prob_blue)
  actual_count <- numeric(100)
  expected_count <- sum(grid = 2)

  # test each individual as well as total count are equivalent
  for (t in 1:100) {
    test_grid_red <- move_red(grid)
    count <- sum(test_grid_red = 2)
    actual_count[t] <- count
    expect_equal(count, expected_count)
  }

  expect_equal(sum(actual_count), expected_count * 100)
})
