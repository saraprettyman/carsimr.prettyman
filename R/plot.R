#' @export
#'
plot.carsimr <- function(x, y, ...) {
  # correcting print orientation
  grid <- x
  if (length(dim(grid)) == 0) {
    stop("Input matrix is empty")
  }
  grid <- grid[seq_len(dim(grid)[1]), ]
  grid <- t(grid)
  dims <- dim(grid)
  r <- dims[1]
  c <- dims[2]
  img <- matrix(NA, nrow = r, ncol = c)
  img[grid == 0] <- 0
  img[grid == 1] <- 1
  img[grid == 2] <- 2
  if (any(grid == 1) && any(grid == 0) && !any(grid == 2)) {
    graphics::image(1:r, 1:c, img,
      col = c("white", "blue"),
      xaxt = "n", yaxt = "n", xlab = "", ylab = ""
    )
  } else {
    graphics::image(1:r, 1:c, img,
      col = c("white", "blue", "red"),
      xaxt = "n", yaxt = "n", xlab = "", ylab = ""
    )
  }
  # Grid lines (border)
  graphics::box()
}

#' @export
plot.carsimr_list <- function(x, y, pause, ...) {
  grid_list <- x
  for (i in seq_along(grid_list)) {
    plot.carsimr(grid_list[[i]])
    Sys.sleep(pause)
  }
}
