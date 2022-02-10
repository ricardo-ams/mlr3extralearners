library(mlr3extralearners)
install_learners("regr.kmeans")

test_that("regr.kmeans train", {
  learner = lrn("regr.kmeans")
  fun = WCLR::kmeans
  exclude = c(
    "object", # handled internally
    "data" # handled internally
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = "")))
})

test_that("regr.kmeans predict", {
  learner = lrn("regr.kmeans")
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
