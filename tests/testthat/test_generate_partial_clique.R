library(testthat)
context("Testing generate_partial_clique")

test_that("generate_partial_clique works", {
  set.seed(10)

  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)

  expect_true(is.list(res)) #checks the output is a list
  expect_true(is.matrix(res$adj_mat)) #checks the adj_mat element is a matrix
  expect_true(all(dim(res$adj_mat) == c(10,10))) #checks the dimensions of the matrix are 10x10
  expect_true(all(diag(res$adj_mat) == 1)) #checks that diagonal of adj_mat == 1

  })

test_that("generate_partial_clique returns adj_mat when clique has 1 node", {
  #checks that cliques of 1 node cannot have edges to other nodes and returns adj_mat correctly in that case
  res <- generate_partial_clique(n = 5,
                                 clique_fraction = 0.1, #forces a single node in the clique since 5*.1 = 1
                                 clique_edge_density = 0.9)

expected_adj_mat <- matrix(0, nrow = 5, ncol=5)
diag(expected_adj_mat) <- 1

  expect_true(identical(res$adj_mat, expected_adj_mat)) #checks that function returns res = list(adj_mat = adj_mat) when there is a single node in the clique

  adj <- res$adj_mat #sets adj to be a matrix extracted from res
  diag(adj) <- 0 #sets off-diagonal values to zero since diagonal value will always connect to itself and be 1
  expect_true(all(adj == 0)) #checks that all entries are zero now

})


