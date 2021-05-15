test_that("S matrix is generated correctly", {
  expected_S_matrix <- matrix(0,2,3)
  colnames(expected_S_matrix) <- paste('arm', 1:3)
  rownames(expected_S_matrix) <- c("average reward","trials")
  expect_identical(generate_matrix_S(3), expected_S_matrix)
})
