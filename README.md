
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

The goal of ‘carsimr.prettyman’ is to run a car simulation, and plot the
results. The simulation is based on a matrix filled with both blue and
red cars, with grey filling in the empty spaces. Red cars move
horizontally rightward and blue cars move vertically upward, with blue
moving on odd turns and red moving on even.

## Installation

You can install the development version of carsimr.prettyman from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("saraprettyman/carsimr.prettyman")
```

Once installed, you can load the package and functions with

``` r
library(carsimr.prettyman)
```

## Example

To initialized grid of dimensions r x c:

``` r
# Positive integer or float are acceptable
r = 3;
c = 5;
rho <- 0.50
dims <- c(r, c)
prob_blue <- .68
grid <- initialize_grid(rho, dims, prob_blue)
grid
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    0    1    0    1
#> [2,]    2    0    2    1    1
#> [3,]    0    1    0    1    0
#> attr(,"class")
#> [1] "carsimr"
```

Using the already created initial grid, the cars of the grid can be
moved for a desire number of times and returns a list of carsimr grids.
Note that the first grid in the list is the initial one.

``` r
moved_list <- move_cars(grid, trials = 4)
```

To plot the results, you can plot the individual grid from
‘initialize_grid’.

<img src="man/figures/README-plot example-1.png" width="100%" />
