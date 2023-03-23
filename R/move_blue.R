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
  # Initialize new grid
  dims <- dim(t_grid)
  new_grid <- matrix(0, dims[1], dims[2])

  # Move the blue cars vertically upward
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      # Test if it is a blue square
      if (t_grid[i, j] == 1) {
        if (i == 1) {
          if (t_grid[dims[1], j] == 0) {
            new_grid[dims[1], j] <- 1
          } else {
            new_grid[i, j] <- 1
          }
        } else {
          if (t_grid[i - 1, j] == 0) {
            new_grid[i - 1, j] <- 1
          } else {
            new_grid[i, j] <- 1
          }
        }
      } else if (t_grid[i, j] == 2) {
        # Copy the red cars to the new grid
        new_grid[i, j] <- 2
      }
    }
  }

  class(new_grid) <- "carsimr"
  return(new_grid)
}
