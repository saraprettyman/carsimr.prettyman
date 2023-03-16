#' Create the initialize grid function to return class instance
#'
#' @param rho - The density of cars in the grid
#' @param dims -  A vector of length 2 representing the dimensions of the grid.
#' @param prob_blue - The proportion of blue squares in the grid,
#' a number between 0 and 1.
#'
#' @return This returns a matrix with blue, red, and no cars represented
#' by 0,1,2 respectively.
#' @export
initialize_grid <- function(rho, dims, prob_blue) {
  p <- c(prob_blue, 1 - prob_blue)
  # calculating the total number of cars, using rho
  if (rho %% 1 != 0 && rho < 1) {
    # if rho is less than 1
    total_cars <- ceiling(rho * dims[1] * dims[2])
  } else {
    total_cars <- as.integer(rho)
  }


  # create total number of red and blue cars
  carz <- sample(c(1, 2), total_cars, prob = p, replace = TRUE)

  # Combinate zeros and cars
  grid_zero <- c(rep(0, (dims[1] * dims[2]) - total_cars))
  grid <- c(grid_zero, carz)
  grid <- matrix(sample(grid), nrow = dims[1], ncol = dims[2])

  # Create a new instance of the carsimr class
  structure(grid, class = "carsimr")
}
