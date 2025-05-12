#' Generate a random adjacency matrix with a partial clique
#'
#' @param n Integer. Number of nodes in the graph.
#' @param clique_fraction Numeric between 0 and 1. Fraction of nodes in the partial clique.
#' @param clique_edge_density Numeric between 0 and 1. Edge density within the partial clique.
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return A list with at least one element:
#' \describe{
#'   \item{adj_mat}{A symmetric binary adjacency matrix with a partial clique.}
#' }
#' @export
generate_partial_clique <- function(n, clique_fraction, clique_edge_density, seed = NULL) {

  # --- Input validation ---
  if (!(is.numeric(n) && length(n) == 1 && n == as.integer(n) && n > 0)) {
    stop("`n` must be a single positive integer.")
  }

  if (!(is.numeric(clique_fraction) && length(clique_fraction) == 1 &&
        clique_fraction >= 0 && clique_fraction <= 1)) {
    stop("`clique_fraction` must be a single number between 0 and 1 (inclusive).")
  }

  if (!(is.numeric(clique_edge_density) && length(clique_edge_density) == 1 &&
        clique_edge_density >= 0 && clique_edge_density <= 1)) {
    stop("`clique_edge_density` must be a single number between 0 and 1 (inclusive).")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # --- Initialize ---
  adj_mat <- matrix(0, nrow = n, ncol = n)
  diag(adj_mat) <- 1

  # --- Determine clique size and nodes ---
  m <- round(n * clique_fraction)
  if (m <= 1) {
    return(list(adj_mat = adj_mat))  # A single node or empty clique has no off-diagonal edges
  }

  clique_nodes <- sample(1:n, m, replace = FALSE)

  # Number of edges in a complete clique of size m
  max_edges <- m * (m - 1) / 2
  n_edges <- round(clique_edge_density * max_edges)

  # Generate all possible edges among clique nodes
  edge_combinations <- combn(clique_nodes, 2, simplify = FALSE)

  if (length(edge_combinations) < n_edges) {
    warning("Requested edge density too high for the clique size; adding all possible edges.")
    selected_edges <- edge_combinations
  } else {
    selected_edges <- sample(edge_combinations, n_edges)
  }

  # Add edges to the adjacency matrix
  for (edge in selected_edges) {
    i <- edge[1]
    j <- edge[2]
    adj_mat[i, j] <- 1
    adj_mat[j, i] <- 1
  }

  # --- Return ---
  return(list(adj_mat = adj_mat))
}
