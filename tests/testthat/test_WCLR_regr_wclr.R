install_learners("regr.wclr")
load_tests("regr.wclr")

test_that("autotest", {
  learner = lrn("regr.wclr")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
