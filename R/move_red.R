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
  # Initialize new grid
  dims <- dim(t_grid)
  new_grid <- matrix(0, dims[1], dims[2])

  # Move the red cars horizontally rightward
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      if (t_grid[i, j] == 2) {
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
      }
    }
  }

  # Copy the blue cars from the original grid to the new grid
  new_grid[t_grid == 1] <- 1

  class(new_grid) <- "carsimr"
  return(new_grid)
}
