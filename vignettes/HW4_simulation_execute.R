rm(list=ls())
set.seed(10)
library(UWBiost561)



imp_numbers = 1:15
trials = 4
alpha_vec = c(0.50, 0.75, 0.95)
n_vec = c(10, 25, 45)

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

      trial_results <- vector("list", length(imp_numbers))

      # loop over the methods for this trial
      result_list <- lapply(imp_numbers, function(imp_number){
        set.seed(trial) # to freeze the randomness of the method
        cat('*')
        result <- UWBiost561::compute_maximal_partial_clique_master(
          adj_mat = adj_mat,
          alpha = alpha,
          number = imp_number,
          time_limit = 30
        )


        return(list(
          status = result$status,
          clique_size = if(!is.null(result$clique_idx)) length(result$clique_idx) else NA,
          edge_density = result$edge_density,
          valid = result$valid,
          time = result$time
        ))

      })

      trial_results <- result_list
      names(trial_results) <- paste("Implementation:", imp_numbers)
      level_results[[trial]] <- trial_results
    }

    names(level_results) <- paste("Trial:", 1:trials)
    level_trial_results[[i]] <- level_results
  }

  names(level_trial_results) <- paste0("alpha:", alpha_vec, "n:", n_vec)
  return(level_trial_results)


  date_of_run <- Sys.time()
  session_info <- devtools::session_info()

  save(level_trial_list, # save your results
       alpha_vec, # save which alphas you used (for convenience)
       date_of_run, session_info,
       file = "~/HW4_simulation.RData")


