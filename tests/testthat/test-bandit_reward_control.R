test_that("visitor_reward is checked correctly", {
  K_too_small <- as.data.frame(matrix(0,5))
  horizon_too_small <- as.data.frame(matrix(0,2,3))
  missing_values <- as.data.frame(cbind(c(NA,1,2),
                                        c(2,3,5)))
  just_fine <- as.data.frame(matrix(0,5,3))

  expect_error(bandit_reward_control(K_too_small),
               "number of arms must be greater than or equal to 2")
  expect_error(bandit_reward_control(horizon_too_small),
               "horizon must be at least equal to the number of arms")
  expect_error(bandit_reward_control(missing_values),
               "missing data in arm results data")
  expect_equal(bandit_reward_control(just_fine), TRUE)
})

test_that("binary reward dataframe is checked correctly", {
  wrong1 <- cbind(c(TRUE,FALSE),c(FALSE,FALSE))
  wrong2 <- matrix(3,3,4)
  wrong3 <- cbind(c("lol", "3"), c(1,1))
  correct1 <- diag(3)
  correct2 <- matrix(data=c(1,1,0,1),nrow=2,ncol=2)

  expect_error(control_binary(wrong1),
               "reward data must be numeric")
  expect_error(control_binary(wrong2),
               "reward data must be binary")
  expect_error(control_binary(wrong3),
               "reward data must be numeric")
  expect_equal(control_binary(correct1), TRUE)
  expect_equal(control_binary(correct2), TRUE)
})
