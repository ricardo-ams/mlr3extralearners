library(mlr3extralearners)
install_learners("regr.wclr")

test_that("regr.wclr train", {
  learner = lrn("regr.wclr")
  fun = WCLR::wclr
  exclude = c(
    "object", # handled internally
    "data" # handled internally
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = "")))
})

test_that("regr.wclr predict", {
  learner = lrn("regr.wclr")
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
