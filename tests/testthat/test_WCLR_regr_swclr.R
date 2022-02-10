install_learners("regr.swclr")
load_tests("regr.swclr")

test_that("autotest", {
  learner = lrn("regr.swclr")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
