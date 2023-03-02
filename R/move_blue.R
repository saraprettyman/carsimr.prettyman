#' @export

move_blue <- function(t_grid) {
    # initialize new grid
    dims <- dim(t_grid)
    new_grid <- matrix(0, dims[1], dims[2])

    # Move the blue cars vertically upward
    for (i in 1:dims[1]) {
        for (j in 1:dims[2]) {
            # testing if it is a blue square
            if (t_grid[i,j] == 1) {
                #checking if in the first row
                if (i == 1) {
                    # checking if last row is available to wrap to
                    if (t_grid[dims[1],j] == 0) {
                        new_grid[dims[1],j] <- 1
                    } else {
                        # keep same position
                        new_grid[i,j] <- 1
                    }
                } else {
                    # checking if can move one cell up
                    if (t_grid[i-1,j] == 0) {
                        new_grid[i-1,j] <- 1
                    } else {
                        # keep same position
                        new_grid[i,j] <- 1
                    }
                }
            }
        }
    }

    # Copy the red cars from the original grid to the new grid
    for (i in 1:dims[1]) {
        for (j in 1:dims[2]) {
            if (t_grid[i,j] == 2) {
                new_grid[i,j] <- 2
            }
        }
    }

    class(new_grid) <- "carsimr"
    return(new_grid)
}
