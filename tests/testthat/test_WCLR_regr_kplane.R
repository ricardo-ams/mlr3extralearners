install_learners("regr.kplane")
load_tests("regr.kplane")

test_that("autotest", {
  learner = lrn("regr.kplane")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
