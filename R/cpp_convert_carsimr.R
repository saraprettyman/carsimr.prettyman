#' Convert a matrix or list object to carsimr class
#'
#' This function takes an object and converts it to the carsimr class. If the
#' object is a list, each element is converted to the carsimr class, and the
#' entire list is returned as a carsimr_list. If the object is a matrix, it is
#' returned as a single carsimr matrix. If the input is not a list or matrix,
#' an error is thrown.
#'
#' @param object A matrix or list object representing a carsimr simulation.
#' @return An object with carsimr class, either a
#' carsimr_list or carsimr matrix.
#'
#' @export
cpp_convert_carsimr <- function(object) {
  if (is.list(object)) {
    class(object) <- "carsimr_list"
    for (i in seq_along(object)) {
      temp <- object[[i]]
      class(temp) <- "carsimr"
      object[[i]] <- temp
    }
  } else if (is.matrix(object)) {
    class(object) <- "carsimr"
  } else {
    stop("Input is not a list or a matrix")
  }
  return(object)
}
