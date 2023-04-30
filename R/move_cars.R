#' Move both cars multiple trials
#'
#' Moves the red and blue cars in a grid for
#' a specified number of trials.
#'
#' @param t_grid A matrix representing the starting grid
#'  with 0 for empty cells, 1 for blue cars, and 2 for red cars.
#' @param trials An integer specifying the number of trials to move the cars.
#' @return A list of matrices representing the grid configuration
#'  after each trial, including the initial grid.
#'
#' @examples
#' grid <- matrix(c(0, 2, 1, 2, 1, 2, 1, 1, 0),
#'   nrow = 3, ncol = 3, byrow = TRUE
#' )
#' move_cars(grid, trials = 5)
#' @export
move_cars <- function(t_grid, trials) {
  # Check input
  if (!is.matrix(t_grid) || all(t_grid != 0 & t_grid != 1 & t_grid != 2)) {
    stop("Input must be a matrix with values 0, 1, or 2")
  }
  if (trials %% 1 != 0 || trials < 1) {
    stop("trials must be a positive integer")
  }

  # Initialize the output list
  car_list <- vector("list", trials + 1)
  car_list[[1]] <- t_grid
  ## Removing for efficiency new_grid <- t_grid

  # Move the cars for the specified number of trials
  for (i in 1:trials) {
    if (i %% 2 == 0) { # Red cars move on even iterations
      t_grid <- move_red(t_grid)
    } else { # Blue cars move on odd iterations
      t_grid <- move_blue(t_grid)
    }
    car_list[[i + 1]] <- t_grid
  }


  # Set class and return
  structure(car_list, class = "carsimr_list")
}
