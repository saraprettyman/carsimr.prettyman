#' @export


# Create the initialize grid function to return class instance
initialize_grid <- function(rho, dims, prob_blue) {
    # calculating the total number of cars, using rho
    if (rho %% 1 != 0 && rho < 1) {
        # if rho is less than 1
        total_cars <- round(rho * dims[1] * dims[2])
    } else {
        total_cars <- as.integer(rho)
    }

    if (total_cars == 0) {
        stop("rho is too small. Please increase rho or choose a larger grid.")
    }


    # create total number of red and blue cars
    if (prob_blue == 0) {
        blue_cars <- 0
        red_cars <- total_cars
    } else if (prob_blue == 1) {
        blue_cars <- total_cars
        red_cars <- 0
    } else {
        blue_cars <- round(total_cars * prob_blue)
        red_cars <- total_cars - blue_cars
    }

    # initialization of matrix
    grid <- matrix(0, dims[1], dims[2])

    # Input blue squares randomly, to one
    if (blue_cars != 0){
        for (i in 1:blue_cars) {
            # Randomize rows and columns
            a <- sample(1:dims[1], 1)
            b <- sample(1:dims[2], 1)
            # Testing if there is nothing there, add
            if (grid[a, b] == 0) {
                grid[a, b] <- 1
            } else {
                i <- i - 1
            }
        }
    }

    # Input red squares randomly, to two
    if (red_cars!= 0){
        for (i in 1:red_cars) {
            # Randomize rows and columns
            a <- sample(1:dims[1], 1)
            b <- sample(1:dims[2], 1)
            # Testing if there is nothing there, add
            if (grid[a, b] == 0) {
                grid[a, b] <- 2
            } else {
                i <- i - 1
            }
        }
    }

    # Create a new instance of the carsimr class
    structure(grid, class = "carsimr")
}
