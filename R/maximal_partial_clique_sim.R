#' Run a simulation of maximal partial clique computing for compute_maximal_partial_clique function
#'
#'
#' @param trials number of trials to be ran for each level per implementation
#' @param alpha_vec vector of alpha levels. Alpha is a numeric threshold between 1 and 0.5 indicating required edge density
#' @param n_vec vector of n levels. n is the number of nodes in the graph
#'
#' @return a nested list with results for each level and each trial
#' @examples
#' trials <- 2
#' alpha_vec <- c(0.50, 0.75, 0.95)
#' n_vec <- c(10, 25, 45)
#' maximal_partial_clique_sim(trials,alpha_vec,n_vec)
#'
#' @export
#'
maximal_partial_clique_sim <- function(
                               trials,
                               alpha_vec,
                               n_vec
                               ) {

stopifnot(length(alpha_vec) == length(n_vec))

level_trial_results <- vector("list", length(alpha_vec))
# loop over the levels
for(i in seq_along(alpha_vec)){
  alpha <- alpha_vec[i]
  n <- n_vec[i]
  print(paste("Value of alpha:", alpha, 'Value of n:', n))

  level_results <- vector("list", trials)

  # loop over the different trials for this level
  for (trial in 1:trials){
    print(paste("Working on trial:", trial))
    set.seed(trial) # to freeze the randomness of adj_mat

    # generate the data
    data <- UWBiost561::generate_partial_clique(n = n, #graph of 10 nodes
                                                clique_fraction = 0.9,
                                                clique_edge_density = 0.9)
    adj_mat <- data$adj_mat

      imp_number <- 1
      set.seed(trial)
      cat('*')

      result <- tryCatch({
        UWBiost561::compute_maximal_partial_clique_master(
          adj_mat = adj_mat,
          alpha = alpha,
          number = imp_number,
          time_limit = 30
        )
      }, error = function(e) {
        message(sprintf("rror in imp_number %d, trial %d: %s",
                        imp_number, trial, e$message))
        list(clique_idx = NA, edge_density = NA, status = "error", valid = FALSE)
      })

      # Ensure no crash due to bad clique_idx
      clique_size <- if (!is.null(result$clique_idx) && is.numeric(result$clique_idx)) {
        length(result$clique_idx)
      } else {
        NA
      }

      trial_results <- list(list(
        status = result$status,
        clique_size = if(!is.null(result$clique_idx)) length(result$clique_idx) else NA,
        edge_density = result$edge_density,
        valid = result$valid,
        time = result$time
      ))

    names(trial_results) <- paste("Implementation:", imp_number)
    level_results[[trial]] <- trial_results
}

  names(level_results) <- paste("Trial:", 1:trials)
level_trial_results[[i]] <- level_results
}

names(level_trial_results) <- paste0("alpha:", alpha_vec, "n:", n_vec)
return(level_trial_results)

}


