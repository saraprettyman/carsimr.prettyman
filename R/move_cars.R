#' Move cars function
#'
#' This function moves the red and blue cars in a grid for
#' a specified number of trials.
#'
#' @param t_grid A matrix representing the starting grid
#' configuration with 0 for empty cells, 1 for blue cars, and 2 for red cars.
#' @param trials An integer specifying the number of trials to move the cars.
#'
#' @return A list of matrices representing the grid configuration
#'  after each trial, including the initial grid.
#' @export

move_cars <- function(t_grid, trials) {
  # initialize the output list
  car_list <- vector("list", trials + 1)
  car_list[[1]] <- t_grid
  new_grid <- t_grid

  # move the cars for the specified number of trials
  for (i in 1:trials) {
    if (i %% 2 == 0) { # Red cars move on even iterations
      new_grid <- move_red(new_grid)
    } else { # Blue cars move on odd iterations
      new_grid <- move_blue(new_grid)
    }
    car_list[[i + 1]] <- new_grid
  }


  # set class and return
  structure(car_list, class = "carsimr_list")
}
