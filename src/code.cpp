#include <Rcpp.h>
using namespace Rcpp;

inline int randWrapper(const int n) { return floor(unif_rand()*n); }

// [[Rcpp::export]]
Rcpp::NumericVector random_shuffle_cpp(Rcpp::NumericVector a) {
    // clone a into b to leave a alone
    Rcpp::NumericVector b = Rcpp::clone(a);
    int n = b.size();
    int j;

    // Fisher-Yates Shuffle Algorithm
    for (int i = 0; i < n - 1; i++) {
        j = i + randWrapper(n - i);
        std::swap(b[i], b[j]);
    }
    return b;
}

//' Create a matrix of values 0, 1, and 2 in C++
//'
//' This function generates a grid of the specified dimensions and
//'  density with randomly distributed red and blue cars.
//'
//' @param rho - The density of cars in the grid
//' @param dims -  A vector of length 2 representing the dimensions of the grid.
//' @param prob_blue - The proportion, between 0 and 1
//' of blue squares out of total number of cars.
//' @return A NumericMatrix representing the grid,
//'  where blue and red cars are represented by 1 and 2,
//'  respectively, and no cars are represented by 0.
//'
//' @examples
//' initialize_grid_cpp(rho = 56, c(10, 10), 0.4)
//' initialize_grid_cpp(rho = 0.50, c(3, 3), 0.96)
//' @name initialize_grid_cpp
//' @export
// [[Rcpp::export]]
NumericMatrix initialize_grid_cpp(double rho, NumericVector dims, double prob_blue){
    // Initialize total cars
    int total_cars;
    if (rho > 0 && rho < 1){
        total_cars = ceil(rho * dims[0] * dims[1]);
    } else {
        total_cars = floor(rho);
    }

    NumericVector x(dims[0]*dims[1]);
    // Create total number of red and blue cars
    for (int i = 0; i < total_cars; i++){
        x(i) = (unif_rand()>(prob_blue)) + 1;
    }
    x = random_shuffle_cpp(x);

    x.attr("dim") = Dimension(dims[0], dims[1]);
    NumericMatrix grid = as<NumericMatrix>(x);
    return grid;
}


// [[Rcpp::export]]
NumericMatrix move_red_cpp(NumericMatrix grid) {
    int nrow = grid.nrow();
    int ncol = grid.ncol();
    NumericMatrix new_grid(nrow, ncol);

    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            if (grid(i, j) == 2) {
                if (j == ncol - 1) {
                    if (grid(i, 0) == 0) {
                        new_grid(i, 0) = 2;
                    } else {
                        new_grid(i, j) = 2;
                    }
                } else {
                    if (grid(i, j + 1) == 0) {
                        new_grid(i, j + 1) = 2;
                    } else {
                        new_grid(i, j) = 2;
                    }
                }
            } else if (grid(i, j) == 1) {
                new_grid(i, j) = 1;
            }
        }
    }

    return new_grid;
}

// [[Rcpp::export]]
NumericMatrix move_blue_cpp(NumericMatrix grid) {
    int nrow = grid.nrow();
    int ncol = grid.ncol();
    NumericMatrix new_grid(nrow, ncol);

    // Move the blue cars vertically upward
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            if (grid(i, j) == 1) {
                if (i == 0) {
                    if (grid(nrow - 1, j) == 0) {
                        new_grid(nrow - 1, j) = 1;
                    } else {
                        new_grid(i, j) = 1;
                    }
                } else {
                    if (grid(i - 1, j) == 0) {
                        new_grid(i - 1, j) = 1;
                    } else {
                        new_grid(i, j) = 1;
                    }
                }
            } else if (grid(i, j) == 2) {
                new_grid(i, j) = 2;
            }
        }
    }

    return new_grid;
}

//' Move both cars multiple trials in C++
//'
//' Moves the red and blue cars in a grid for
//' a specified number of trials in C++.
//'
//' @param grid A NumericMatrix representing the starting grid
//' with 0 for empty cells, 1 for blue cars, and 2 for red cars.
//' @param trials An integer specifying the number of trials to move the cars.
//' @return A List of NumericMatrices representing the grid configuration
//' after each trial, including the initial grid.
//' @name move_cars_cpp
//' @export
// [[Rcpp::export]]
List move_cars_cpp(NumericMatrix grid, int trials) {
    List car_list(trials + 1);
    car_list[0] = clone(grid);

    // Initialize each element of car_list with a copy of grid
    for (int i = 1; i <= trials; i++) {
        if (i % 2 == 0){
            grid = move_red_cpp(grid);
        } else {
            grid = move_blue_cpp(grid);
        }
        car_list[i] = clone(grid);
    }

    return car_list;
}
