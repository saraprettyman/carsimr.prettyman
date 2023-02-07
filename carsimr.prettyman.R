initialize_grid <- function(rho, dims, prob_blue) {
  # calculating the total number of cars, using rho
  if (rho %% 1 != 0 && rho < 1) {
    # if rho is less than 1
    total_cars <- round(rho * dims[1] * dims[2])
  } else {
    total_cars <- as.integer(rho)
  }
  # create total number of red and blue cars
  blue_cars <- round(total_cars * prob_blue)
  red_cars <- total_cars - blue_cars

  # initialization of matrix
  grid <- matrix(0, dims[1], dims[2])

  # Input blue squares randomly
  for (i in 1:blue_cars) {
    # Randomize rows and columns
    a <- sample(1:dims[1], 1)
    b <- sample(1:dims[2], 1)
    # Testing if there is nothing there, if so paste
    if (grid[a, b] == 0) {
      grid[a, b] <- 1
    } else {
      i <- i - 1
    }
  }

  # Input red squares randomly
  for (i in 1:red_cars) {
    # Randomize rows and columns
    a <- sample(1:dims[1], 1)
    b <- sample(1:dims[2], 1)
    # Testing if there is nothing there, if so paste
    if (grid[a, b] == 0) {
      grid[a, b] <- 2
    } else {
      i <- i - 1
    }
  }
  return(grid)
}
