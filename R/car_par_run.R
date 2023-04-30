car_par_run <- function(rho, dims, prob_blue, trials, replicates, cores = 1) {
  if (cores == 1) {
    results <- lapply(1:replicates, function(x) {
      grid <- initialize_grid(rho, dims, prob_blue)
      move_cars(grid, trials)
    })
  } else {
    # Initialize cluster based on user-provided cores
    cl <- parallel::makeCluster(cores)

    # Exporting necessary variables in the local environment
    parallel::clusterExport(cl, c("initialize_grid", "move_cars", "rho", "dims", "prob_blue", "trials"), envir = environment())

    # Run in parallel
    results <- parallel::parLapply(cl, 1:replicates, function(x) {
      grid <- initialize_grid(rho, dims, prob_blue)
      move_cars(grid, trials)
    })


    on.exit(parallel::stopCluster(cl))

    return(results)
  }
}
