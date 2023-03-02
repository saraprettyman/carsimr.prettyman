#' @export


move_cars <- function(t_grid, trials) {

    # initialize the output list
    car_list <- vector("list", trials+1)
    car_list[[1]] <- t_grid
    new_grid <- t_grid

    # move the cars for the specified number of trials
    for (i in 1:trials) {
        new_grid_red <- move_red(new_grid)
        new_grid_red_blue <- move_blue(new_grid_red)
        new_grid <- new_grid_red_blue
        car_list[[i+1]] <- new_grid
    }

    # set class and return
    structure(car_list, class = "carsimr_list")
}
