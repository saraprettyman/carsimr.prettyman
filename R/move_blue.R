#' Move blue cars upward in a grid
#'
#' This function moves the blue cars upward in a grid.
#' It takes a matrix as input and returns a new matrix
#' with the blue cars moved one cell up if possible.
#' If a blue car is in the top row, it is moved to the
#' bottom row if the cell in the same column in the bottom row
#'  is empty. Red cars are not moved.
#'
#' @param t_grid A matrix representing the initial configuration
#' of the cars.
#' @return A matrix representing the configuration of the
#' cars after moving the blue cars upward.
#' @export

move_blue <- function(t_grid) {
  # initialize new grid
  dims <- dim(t_grid)
  new_grid <- matrix(0, dims[1], dims[2])

  # Move the blue cars vertically upward
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      # testing if it is a blue square
      if (t_grid[i, j] == 1) {
        new_grid <- move_blue_car(i, j, t_grid, new_grid, dims)
      }
    }
  }

  # Copy the red cars from the original grid to the new grid
  new_grid[t_grid == 2] <- 2

  class(new_grid) <- "carsimr"
  return(new_grid)
}

#' Move a single blue car upward in a grid
#'
#' This function moves a single blue car upward in a grid.
#' If the car is in the top row, it is moved to the bottom row
#' if the cell in the same column in the bottom row is empty.
#' If the car can move one cell up, it is moved to that cell.
#' Otherwise, it stays in the same position.
#'
#' @param i The row index of the car.
#' @param j The column index of the car.
#' @param t_grid The original grid.
#' @param new_grid The new grid being constructed.
#' @param dims The dimensions of the grid.
#' @return The updated new grid.
move_blue_car <- function(i, j, t_grid, new_grid, dims) {
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
  return(new_grid)
}
