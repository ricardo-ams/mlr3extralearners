library(mlr3extralearners)
install_learners("regr.kplane")

test_that("regr.kplane train", {
  learner = lrn("regr.kplane")
  fun = WCLR::kplane
  exclude = c(
    "object", # handled internally
    "data" # handled internally
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = "")))
})

test_that("regr.kplane predict", {
  learner = lrn("regr.kplane")
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
