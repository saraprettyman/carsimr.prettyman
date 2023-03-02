#' @export

move_red <- function(t_grid) {
    # initialize new grid
    dims <- dim(t_grid)
    new_grid <- matrix(0, dims[1], dims[2])

    # Move the red cars horizontally rightward
    for (i in 1:dims[1]) {
        for (j in 1:dims[2]) {
            if (t_grid[i, j] == 2) {
                # if at end of row
                if (j == dims[2]) {
                    # if beginning of row is available and not blue
                    if (t_grid[i, 1] == 0) {
                        new_grid[i, 1] <- 2
                    } else {
                        new_grid[i, j] <- 2
                    }
                } else {
                    # if next cell is free
                    if (t_grid[i, j + 1] == 0) {
                        new_grid[i, j + 1] <- 2
                    } else {
                        # stay the same if can't move
                        new_grid[i, j] <- 2
                    }
                }
            }
        }
    }

    # Copy the blue cars from the original grid to the new grid
    for (i in 1:dims[1]) {
        for (j in 1:dims[2]) {
            if (t_grid[i, j] == 1) {
                new_grid[i, j] <- 1
            }
        }
    }

    class(new_grid) <- "carsimr"
    return(new_grid)
}
