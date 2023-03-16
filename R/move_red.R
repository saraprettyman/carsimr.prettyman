#' Move the red cars horizontally rightward
#'
#' This function moves the red cars in a traffic simulation grid
#' horizontally rightward by one unit if there is no car in the way.
#' Blue cars are not moved. The function returns a new grid with the
#' updated positions of the cars.
#'
#' @param t_grid The traffic simulation grid to be updated.
#'
#' @return A new grid with the updated positions of the cars.
#' @export
move_red <- function(t_grid) {
  # initialize new grid
  dims <- dim(t_grid)
  new_grid <- matrix(0, dims[1], dims[2])

  # Move the red cars horizontally rightward
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      if (t_grid[i, j] == 2) {
        new_grid <- move_red_car(i, j, t_grid, new_grid, dims)
      }
    }
  }

  # Copy the blue cars from the original grid to the new grid
  new_grid[t_grid == 1] <- 1

  class(new_grid) <- "carsimr"
  return(new_grid)
}

#' Move a single red car horizontally rightward in a grid
#'
#' This function moves a single red car horizontally rightward in a grid.
#' If the car is in the rightmost column, it is moved to the leftmost column
#' if the cell in the same row in the leftmost column is empty.
#' If the car can move one cell rightward, it is moved to that cell.
#' Otherwise, it stays in the same position.
#'
#' @param i The row index of the car.
#' @param j The column index of the car.
#' @param t_grid The original grid.
#' @param new_grid The new grid being constructed.
#' @param dims The dimensions of the grid.
#' @return The updated new grid.
move_red_car <- function(i, j, t_grid, new_grid, dims) {
  if (j == dims[2]) {
    if (t_grid[i, 1] == 0) {
      new_grid[i, 1] <- 2
    } else {
      new_grid[i, j] <- 2
    }
  } else {
    if (t_grid[i, j + 1] == 0) {
      new_grid[i, j + 1] <- 2
    } else {
      new_grid[i, j] <- 2
    }
  }
  return(new_grid)
}
