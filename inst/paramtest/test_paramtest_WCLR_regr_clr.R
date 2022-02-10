library(mlr3extralearners)
install_learners("regr.clr")

test_that("regr.clr train", {
  learner = lrn("regr.clr")
  fun = WCLR::clr
  exclude = c(
    "object", # handled internally
    "data" # handled internally
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = "")))
})

test_that("regr.clr predict", {
  learner = lrn("regr.clr")
  fun = WCLR:::predict # nolint
    exclude = c(
      "object", # handled internally
      "data", # handled internally
      "newdata" # handled internally
    )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = "")))
})
