#' Create a matrix of values 0, 1, and 2
#'
#' @param rho - The density of cars in the grid
#' @param dims -  A vector of length 2 representing the dimensions of the grid.
#' @param prob_blue - The proportion, between 0 and 1
#' of blue squares out of total number of cars.
#'
#' @return Returns a matrix representing the grid,
#' where blue and red cars are represented by 1 and 2,
#' respectively, and no cars are represented by 0.
#' @examples
#' initialize_grid(rho = 56, c(10, 10), 0.4)
#' initialize_grid(rho = 0.50, c(3, 3), 0.96)
#' @export
initialize_grid <- function(rho, dims, prob_blue) {
  p <- c(prob_blue, 1 - prob_blue)

  # Calculating the total number of cars, using rho
  if (rho %% 1 != 0 && rho < 1) {
    total_cars <- ceiling(rho * dims[1] * dims[2])
  } else {
    total_cars <- as.integer(rho)
  }


  # Create total number of red and blue cars
  carz <- sample(c(1, 2), total_cars, prob = p, replace = TRUE)

  # Combine zeros and cars
  grid_zero <- c(rep(0, (dims[1] * dims[2]) - total_cars))
  grid <- c(grid_zero, carz)
  grid <- matrix(sample(grid), nrow = dims[1], ncol = dims[2])

  # Create a new instance of the carsimr class
  structure(grid, class = "carsimr")
}
