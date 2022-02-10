install_learners("regr.clr")
load_tests("regr.clr")

test_that("autotest", {
  learner = lrn("regr.clr")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
