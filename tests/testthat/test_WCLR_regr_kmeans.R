install_learners("regr.kmeans")
load_tests("regr.kmeans")

test_that("autotest", {
  learner = lrn("regr.kmeans")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
