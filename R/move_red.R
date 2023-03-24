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
  # Check input
  if (!is.matrix(t_grid) || all(t_grid != 0 & t_grid != 1 & t_grid != 2)) {
    stop("Input must be a matrix with values 0, 1, or 2")
  }

  # Check if there are any red cars
  if (!any(t_grid == 2)) {
    return(t_grid)
  }

  # Initialize new grid
  dims <- dim(t_grid)
  new_grid <- matrix(0, dims[1], dims[2])



  # Move the red cars horizontally rightward
  red_cars <- which(t_grid == 2, arr.ind = TRUE)
  for (i in seq_len(dim(red_cars)[1])) {
    # Check if the next column is free
    if (red_cars[i, 2] == dims[2]) {
      if (t_grid[red_cars[i, 1], 1] == 0) {
        new_grid[red_cars[i, 1], 1] <- 2
      } else {
        new_grid[red_cars[i, 1], red_cars[i, 2]] <- 2
      }
    } else {
      if (t_grid[red_cars[i, 1], red_cars[i, 2] + 1] == 0) {
        new_grid[red_cars[i, 1], red_cars[i, 2] + 1] <- 2
      } else {
        new_grid[red_cars[i, 1], red_cars[i, 2]] <- 2
      }
    }
  }

  # Copy the blue cars from the original grid to the new grid
  new_grid[t_grid == 1] <- 1

  class(new_grid) <- "carsimr"
  return(new_grid)
}
