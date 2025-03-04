library(mlr3extralearners)
mlr3extralearners::install_learners("regr.IBk")

test_that("regr.IBk", {
  learner = lrn("regr.IBk")
  fun = RWeka::IBk
  exclude = weka_control_args(RWeka::IBk)
  # formula and data are handled via mlr3
  # mlr3 does not have the `control` argument because the parameters can be specified directly
  exclude = c("formula", "data", "control", exclude)
  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest)
})

test_that("Weka_control Bk", {
  # Here we test that the learner implements those arguments that are passed via the
  # control argument to RWeka::IBk
  learner = lrn("regr.IBk")
  control_args = weka_control_args(RWeka::IBk)
  expect_true(all(control_args %in% learner$param_set$ids()))
})


test_that("Parameters can be set", {
  learner = lrn("regr.IBk")
  learner$param_set$values = learner$param_set$default
  task = tsk("boston_housing")
  expect_error(learner$train(task), NA)
})
