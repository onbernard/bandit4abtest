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
