#' Move blue cars upward in a grid
#'
#' This function moves the blue cars upward in a grid.
#' It takes a matrix as input and returns a new matrix
#' with the blue cars moved one cell up if possible.
#' If a blue car is in the top row, it is moved to the
#' bottom row if the cell in the same column in the bottom row
#' is empty. Red cars are not moved.
#'
#' @param t_grid A matrix representing the initial configuration
#' of the cars.
#' @return A matrix representing the configuration of the
#' cars after moving the blue cars upward.
#' @export
move_blue <- function(t_grid) {
  # Check input
  if (!is.matrix(t_grid) || all(t_grid != 0 & t_grid != 1 & t_grid != 2)) {
    stop("Input must be a matrix with values 0, 1, or 2")
  }

  # Check if there are any blue cars
  if (!any(t_grid == 1)) {
    return(t_grid)
  }

  # Initialize new grid
  dims <- dim(t_grid)
  new_grid <- matrix(0, dims[1], dims[2])

  # Move the blue cars vertically upward
  blue_cars <- which(t_grid == 1, arr.ind = TRUE)
  for (i in seq_len(dim(blue_cars)[1])) {
    # Check if it is in the top row
    if (blue_cars[i, 1] == 1) {
      # Check if the bottom row is free
      if (t_grid[dims[1], blue_cars[i, 2]] == 0) {
        new_grid[dims[1], blue_cars[i, 2]] <- 1
      } else {
        new_grid[blue_cars[i, 1], blue_cars[i, 2]] <- 1
      }
    } else {
      # Check if the next row is free
      if (t_grid[blue_cars[i, 1] - 1, blue_cars[i, 2]] == 0) {
        new_grid[blue_cars[i, 1] - 1, blue_cars[i, 2]] <- 1
      } else {
        new_grid[blue_cars[i, 1], blue_cars[i, 2]] <- 1
      }
    }
  }

  # Copy the red cars to the new grid
  new_grid[t_grid == 2] <- 2

  class(new_grid) <- "carsimr"
  return(new_grid)
}
