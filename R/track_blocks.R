track_blocks <- function(carsimr_list) {
  if (!inherits(carsimr_list, "carsimr_list")) {
    stop("Input must be a carsimr_list object")
  }

  n_iterations <- (length(carsimr_list) - 1) / 2
  blue <- numeric(0)
  red <- numeric(0)

  for (i in 1:(length(carsimr_list) - 1)) {
    prev_grid <- carsimr_list[[i]]
    curr_grid <- carsimr_list[[i + 1]]

    # Identify the cars that did not move
    not_moved <- prev_grid == curr_grid

    # Blue cars move on odd iterations
    if (i %% 2 == 1) {
      blue_cars_not_moved <- not_moved & prev_grid == 1
      total_blue_cars <- sum(prev_grid == 1)
      if (total_blue_cars == 0) {
        blue <- append(blue, 0)
      } else {
          blue <-append(blue, sum(blue_cars_not_moved) / total_blue_cars)
      }
    } else { # Red cars move on even iterations
      red_cars_not_moved <- not_moved & prev_grid == 2
      total_red_cars <- sum(prev_grid == 2)
      if (total_red_cars == 0) {
          red<-append(red, 0)
      } else {
          red<-append(red,  sum(red_cars_not_moved) / total_red_cars)
      }
    }
  }

  list(blue = blue, red = red)
}

# Helper function for check
to_carsimr_list <- function(test_data) {
  class(test_data) <- c("carsimr")
  return(test_data)
}
