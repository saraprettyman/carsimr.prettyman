#'
#' @export


plot.carsimr <- function(grid) {
    # correcting print orientation
    grid <- grid[dim(grid)[1]:1, ]
    grid <- t(grid)
    dims <- dim(grid)
    r <- dims[1]
    c <- dims[2]
    img <- matrix(NA, nrow = r, ncol = c)
    img[grid == 0] <- 0
    img[grid == 1] <- 1
    img[grid == 2] <- 2
    if (any(grid == 1) && any(grid == 0) && !any(grid == 2)){
        graphics::image(1:r, 1:c, img, col=c("white", "blue"), xaxt="n", yaxt="n", xlab="", ylab="")
    }else{
        graphics::image(1:r, 1:c, img, col=c("white", "blue", "red"), xaxt="n", yaxt="n", xlab="", ylab="")
    }
    # Grid lines (border)
    graphics::box()
}

plot.carsimr_list <- function(grid_list, pause){
    for (i in 1:length(grid_list)){
        plot.carsimr(grid_list[[i]])
        Sys.sleep(pause)
    }
}
