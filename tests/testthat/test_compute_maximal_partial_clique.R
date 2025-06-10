library(testthat)

context("Testing compute_maximal_partial_clique")

#checking that the function outputs something that is the correct type
test_that("compute_maximal_partial_clique works", {
  set.seed(10)

  simulation <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)

  adj_mat <- simulation$adj_mat

  res <- compute_maximal_partial_clique1(
    adj_mat = adj_mat,
    alpha = 0.9
  )
  res

  expect_true(is.list(res)) #checks the output is a list
  expect_true("clique_idx" %in% names(res)) #checks that clique_idx is in res list
  expect_true("edge_density" %in% names(res)) #checks that edge_density is in res list
  expect_true(is.numeric(res$clique_idx)) #checks that clique_idx returns a vector
  expect_true(is.numeric(res$edge_density))
  expect_true(length(res$edge_density) == 1) #checks that edge_density is a numeric value of length 1

})

  #testing that get_density returns correctly - checking corner cases
test_that("get_density returns correct density for fully connected subgraph", {

  #defines get_density function first
  get_density <- function(subgraph) { #use get_density function in compute_maximal_partial_clique
    m <- length(subgraph)
    if (m < 2) return(1)
    total_possible_edges <- m * (m - 1) / 2
    total_edges <- sum(adj_mat[subgraph, subgraph]) - m  # exclude diagonals
    total_edges <- total_edges / 2  # undirected
    return(total_edges / total_possible_edges)
  }


  set.seed(10)

  simulation <- generate_partial_clique(n = 10,
                                        clique_fraction = 0.5,
                                        clique_edge_density = 0.9)

  adj_mat <- simulation$adj_mat


  subgraph_3 <- c(1,2,3) #creates subgraph of 1,2,3


  # Only continue test if subgraph_3 is fully connected
  if (adj_mat[1, 2] == 1 && adj_mat[1, 3] == 1 && adj_mat[2, 3] == 1) {
    calculated_density <- get_density(subgraph_3)#if fully connected, calculate density of the subgroup of adj_mat
    expect_equal(calculated_density, 1) #check that it is equal to correct density of 1 (since fully connected)
  } else { #otherwise skip density test
    message("Subgraph {1,2,3} is not fully connected in this random graph. Skipping density = 1 check.")
  }


})

#testing to make sure it errors when expected
test_that("returns errors if adj_mat is invalid", {

#checks that it returns error when adj_mat contains values other than 0 or 1
invalid_adj_mat1 <- matrix(c(1,2,2,1), nrow = 2, byrow = TRUE)

expect_error(compute_maximal_partial_clique1(invalid_adj_mat1, alpha = 0.9))

#checks that it returns error when adj_mat has values other than 1 on diagonal
invalid_adj_mat2 <- matrix(c(0, 1, 1, 0), nrow=2, byrow = TRUE)

expect_error(compute_maximal_partial_clique1(invalid_adj_mat2, alpha = 0.9))

})


#making sure the function gets the correct answer for carefully crafted problem
test_that("compute_maximal_partial_clique is correct", {

  adj_mat <- matrix(c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1), nrow = 5, byrow=TRUE)

  expected_clique_idx <- c(3, 4, 5)
  expected_edge_density <- 2*2 / (3*(3-1))

  res <- compute_maximal_partial_clique1(
    adj_mat = adj_mat,
    alpha = 0.6
  )
  res

  expect_equal(res$clique_idx, expected_clique_idx) #make sure the clique id from the function is same as known clique id
  expect_equal(res$edge_density, expected_edge_density) #make sure the edge density from function is same as calculated edge density

})

#making sure function runs on many different input (or same input if randomness)

test_that("same output for repeated adjacency matrix", {

    set.seed(10)

    simulation1 <- generate_partial_clique(n = 10,
                                          clique_fraction = 0.5,
                                          clique_edge_density = 0.9)

    adj_mat1 <- simulation1$adj_mat

set.seed(10)
    res1 <- compute_maximal_partial_clique1(
      adj_mat = adj_mat1,
      alpha = 0.9
    )

set.seed(10)
      simulation2 <- generate_partial_clique(n = 10,
                                            clique_fraction = 0.5,
                                            clique_edge_density = 0.9)

      adj_mat2 <- simulation2$adj_mat

set.seed(10)
      res2 <- compute_maximal_partial_clique1(
        adj_mat = adj_mat2,
        alpha = 0.9
      )

      expect_equal(res1$clique_idx, res2$clique_idx)
      expect_equal(res1$edge_density, res2$edge_density)


  })
