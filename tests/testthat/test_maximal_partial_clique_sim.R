library(testthat)


context("testing maximal_partial_clique_sim")


  test_that("Level result has correct output", {
    set.seed(10)
    res <- maximal_partial_clique_sim(
      imp_numbers = 2,
      trials = 1,
      alpha_vec = c(0.9),
      n_vec = c(10)
    )
    trial1_result <- res[[1]][["Trial: 1"]]
    expect_true(is.list(trial1_result))
  })

  test_that("Trial result has correct output", {
    res <- maximal_partial_clique_sim(
      imp_numbers = 2,
      trials = 1,
      alpha_vec = c(0.9),
      n_vec = c(10)
    )
    implementation1_result <- res[[1]][["Trial: 1"]][[1]]
    expect_true(is.list(implementation1_result))
    expect_true(all(c("status", "clique_size", "edge_density", "valid", "time") %in% names(implementation1_result)))
  })


test_that("partial_clique_sim runs on simple case", {
  res <- maximal_partial_clique_sim(
    imp_numbers = 1:2,
    trials = 1,
    alpha_vec = c(0.9),
    n_vec = c(10)
  )

  expect_true(is.list(res))
  expect_named(res, "alpha:0.9n:10")

  trial_result <- res[[1]][["Trial: 1"]]
  expect_length(trial_result, 2)
  expect_true(is.list(trial_result[[1]]))
})


test_that("valid returns TRUE or FALSE", {
  res <- maximal_partial_clique_sim(
    imp_numbers = 1,
    trials = 1,
    alpha_vec = c(0.5),
    n_vec = c(10)
  )

  val <- res[[1]][["Trial: 1"]][[1]]$valid
  expect_true(is.logical(val))
})



