#' Constructor
#'
#' @param x - object
#'
#' @export
#'
new_carsimr <- function(x) {
  structure(x, class = "carsimr")
}

# Validator carsimr
validate_carsimr <- function(x) {
  if (!is.matrix(x)) {
    stop("object must be a matrix")
  }
}

# Helper Function carsimr
carsimr <- function(x) {
  validate_carsimr(x)
  new_carsimr(x)
}


# Constructor carsimr_list
new_carsimr_list <- function(grid_list) {
  class(grid_list) <- "carsimr_list"
  list(grid_list = grid_list)
}

# Validator carsimr_list
validate_carsimr_list <- function(object) {
  if (!is.list(object) || length(object) == 0) {
    stop("grid_list must be a non-empty list.")
  }
}

# Helper Function carsimr_list
carsimr_list <- function(x) {
  validate_carsimr_list(x)
  new_carsimr_list(x)
}
