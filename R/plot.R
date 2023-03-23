#' Plot carsimr output
#'
#' Plot an image of the carsimr grid with
#' blue and red colors representing the cars.
#'
#'
#' @param x A matrix that contains the carsimr output
#' @param y Ignore
#' @param ... (optional) Additional arguments for the
#' graphics::image function
#' @import lattice
#'
#' @return An image of the carsimr matrix
#' @examples
#' grid <- initialize_grid(0.50, c(3,3), 0.5)
#' plot(grid)
#' @export
plot.carsimr <- function(x, y, ...) {
  # correcting print orientation
  grid <- x
  if (length(dim(grid)) == 0) {
    stop("Input matrix is empty")
  }
  grid <- grid[seq_len(dim(grid)[1]), ]
  grid <- t(grid)
  lattice::levelplot(grid,
    cuts = 2,
    xlim = c(0.5, ncol(x) + 0.5),
    ylim = c(nrow(x) + 0.5, 0.5),
    scale = list(draw = FALSE),
    ylab = "",
    xlab = "",
    colorkey = FALSE,
    labels = FALSE,
    panel = function(...) {
      lattice::panel.levelplot(...)
      lattice::panel.grid(v = ncol(x) - 1, h = nrow(x) - 1, col = "black")
    },
    col.regions = c("grey", "blue", "red"), at = c(-0.1, 0.9, 1.9, 2.1)
  )
}

#' Plot List of carsimr output
#'
#' Plots a list of carsimr outputs, each containing
#' an image with blue and red representing the cars
#'
#' @param x A list of matrices containing the carsimr outputs
#' @param y Ignored
#' @param pause The pause time (in seconds) between each
#' plot image print
#' @param ... (optional) Additional arguments for the plot.carsimr function
#'
#' @return A sequence of plots protraying the carsimr outputs
#' @examples
#' grid <- initialize_grid(0.50, c(3,3), 0.5)
#' grid_list <- move_cars(grid, 5)
#' plot(grid_list, pause = 1)
#' @export
plot.carsimr_list <- function(x, y, pause, ...) {
  grid_list <- x
  for (i in seq_along(grid_list)) {
    plot.carsimr(grid_list[[i]])
    Sys.sleep(pause)
  }
}
