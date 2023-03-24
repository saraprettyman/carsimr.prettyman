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
#' grid <- initialize_grid(0.50, c(3, 3), 0.5)
#' plot(grid)
#' @export
plot.carsimr <- function(x, y, ...) {
  if (!inherits(x, "carsimr") || !is.matrix(x)) {
    stop("Input must be a carsimr object and matrix")
  }
  # Check input matrix
  if (length(dim(x)) == 0) {
    warning("Input matrix is empty")
  }

  # Correct print orientation
  grid <- t(x[seq_len(dim(x)[1]), ])

  # Plot image
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
    col.regions = c("grey", "blue", "red"), at = c(-0.1, 0.9, 1.9, 2.1),
    ...
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
#' grid <- initialize_grid(0.50, c(3, 3), 0.5)
#' grid_list <- move_cars(grid, 3)
#' plot(grid_list, pause = 1)
#' @export
plot.carsimr_list <- function(x, y, pause, ...) {
  # Check input list
  if (!is.list(x) || !inherits(x, "carsimr_list")) {
    stop("Input must be a list")
  }

  if (length(x) == 0) {
    warning("Input list is empty")
  }

  if (pause < 0) {
    stop("Time must be non-negative")
  }

  # Plot each matrix in the list
  for (i in seq_along(x)) {
    plot.carsimr(x[[i]], ...)
    Sys.sleep(pause)
  }
}
