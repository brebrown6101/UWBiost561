# Please run this after running hw4_simulation_execute.slurm
rm(list = ls())

# Load simulation results
load("~/HW4_simulation.RData")


library(ggplot2)
library(cowplot)


ggplot_list <- lapply(seq_along(level_trial_results), function(level_number) {
  trial_list <- level_trial_results[[level_number]]
  alpha <- alpha_vec[level_number]
  n <- n_vec[level_number]

  # For each trial, identify which implementations found the largest valid clique
  trial_mat <- sapply(trial_list, function(trial_results) {

    #check for valid implementations
    valid_vec <- sapply(trial_results, function(x) x$valid)
    #check clique size of valid implementations
    clique_sizes <- sapply(trial_results, function(x) if (x$valid) x$clique_size else NA)
    #check for largest
    max_size <- max(clique_sizes, na.rm = TRUE)
    #establish who had largest clique (could be multiple implementations guess it right)
    winner_vec <- sapply(clique_sizes, function(size) !is.na(size) && size == max_size)

    return(winner_vec)
  })

  # Count number of wins per implementation
  tabulate_vec <- rowSums(trial_mat)

  method_names <- as.character(1:15)
  df <- data.frame(method = method_names, number_wins = tabulate_vec)

  #bar plot of wins per implementation
  gg <- ggplot(df, aes(x = method, y = number_wins)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(limits = method_names) +
    labs(
      x = "Implementation",
      y = "Number of wins",
      title = paste("For alpha =", alpha, "and n =", n)
    )

  return(gg)
})

# Combine plots into one image
plot_all <- cowplot::plot_grid(plotlist = ggplot_list, ncol = 1)

# Save the plot to a PNG file
ggsave(filename = "~/UWBiost561/vignettes/HW4_simulation.png",
       plot = plot_all,
       height = 7 * length(ggplot_list), width = 9, units = "in")
