% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.carsimr}
\alias{plot.carsimr}
\title{Plot carsimr output}
\usage{
\method{plot}{carsimr}(x, y, ...)
}
\arguments{
\item{x}{A matrix that contains the carsimr output}

\item{y}{Ignore}

\item{...}{(optional) Additional arguments for the
graphics::image function}
}
\value{
An image of the carsimr matrix
}
\description{
Plot an image of the carsimr grid with
blue and red colors representing the cars.
}
\examples{
grid <- initialize_grid(0.50, c(3, 3), 0.5)
plot(grid)
}
