% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_maximal_partial_clique.R
\name{compute_maximal_partial_clique}
\alias{compute_maximal_partial_clique}
\title{Compute Maximal Partial Clique}
\usage{
compute_maximal_partial_clique(adj_mat, alpha, verbose = FALSE)
}
\arguments{
\item{adj_mat}{A symmetric 0/1 adjacency matrix with 1s on the diagonal.}

\item{alpha}{A numeric threshold between 0.5 and 1 indicating required edge density.}

\item{verbose}{Logical; whether to print debug output. Default is FALSE.}
}
\value{
A list with:
\describe{
\item{clique_idx}{Vector of node indices in the maximal partial clique.}
\item{edge_density}{Density of the subgraph induced by clique_idx.}
}
}
\description{
Compute Maximal Partial Clique
}
