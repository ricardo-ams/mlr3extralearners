#' @title Regression Gradient Boosting Machine Learner
#' @author be-marc
#'
#' @name mlr_learners_regr.gbm
#'
#' @template class_learner
#' @templateVar id regr.gbm
#' @templateVar caller gbm
#'
#' @section Custom mlr3 defaults:
#' - `keep_data`:
#'   - Actual default: TRUE
#'   - Adjusted default: FALSE
#'   - Reason for change: `keep_data = FALSE` saves memory during model fitting.
#' - `n.cores`:
#'   - Actual default: NULL
#'   - Adjusted default: 1
#'   - Reason for change: Suppressing the automatic internal parallelization if
#'     `cv.folds` > 0.
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerRegrGBM = R6Class("LearnerRegrGBM",
  inherit = LearnerRegr,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        distribution = p_fct(default = "gaussian", levels = c("gaussian", "laplace", "poisson", "tdist", "quantile"), tags = "train"),
        n.trees = p_int(default = 100L, lower = 1L, tags = c("train", "predict", "importance")),
        interaction.depth = p_int(default = 1L, lower = 1L, tags = "train"),
        n.minobsinnode = p_int(default = 10L, lower = 1L, tags = "train"),
        shrinkage = p_dbl(default = 0.001, lower = 0, tags = "train"),
        bag.fraction = p_dbl(default = 0.5, lower = 0, upper = 1, tags = "train"),
        train.fraction = p_dbl(default = 1, lower = 0, upper = 1, tags = "train"),
        cv.folds = p_int(default = 0L, tags = "train"),
        alpha = p_dbl(default = 0.5, lower = 0, upper = 1, tags = "train"),
        # Set to FALSE to reduce memory requirements
        keep.data = p_lgl(default = FALSE, tags = "train"),
        verbose = p_lgl(default = FALSE, tags = "train"),
        # Set to 1 to suppress parallelization by the package
        n.cores = p_int(default = 1, tags = "train"),
        var.monotone = p_uty(tags = "train")
      )
      ps$values = list(keep.data = FALSE, n.cores = 1)

      ps$add_dep("alpha", "distribution", CondEqual$new("quantile"))

      super$initialize(
        id = "regr.gbm",
        packages = c("mlr3extralearners", "gbm"),
        feature_types = c("integer", "numeric", "factor", "ordered"),
        predict_types = "response",
        param_set = ps,
        properties = c("weights", "importance", "missings"),
        man = "mlr3extralearners::mlr_learners_regr.gbm"
      )
    },

    #' @description
    #' The importance scores are extracted by `gbm::relative.influence()` from
    #' the model.
    #'
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stop("No model stored")
      }
      pars = self$param_set$get_values(tags = "importance")
      # n.trees is required for prediction. If not set by the user, we take the
      # default (100)
      if (is.null(self$param_set$values$n.trees)) {
        pars$n.trees = self$param_set$default$n.trees # nolint
      }

      imp = mlr3misc::invoke(gbm::relative.influence, self$model, .args = pars)
      sort(imp, decreasing = TRUE)
    }
  ),
  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tags = "train")
      f = task$formula()
      data = task$data()

      if (!is.null(pars$distribution)) {
        if (pars$distribution == "quantile") {
          alpha = ifelse(is.null(pars$alpha), 0.5, pars$alpha)
          pars$distribution = list(name = "quantile", alpha = alpha)
        }
      }

      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }

      mlr3misc::invoke(gbm::gbm, formula = f, data = data, .args = pars)
    },

    .predict = function(task) {
      pars = self$param_set$get_values(tags = "predict")
      # n.trees is required for prediction. If not set by the user, we take the
      # default (100)
      if (is.null(self$param_set$values$n.trees)) {
        pars$n.trees = self$param_set$default$n.trees # nolint
      }
      newdata = task$data(cols = task$feature_names)

      p = mlr3misc::invoke(predict, self$model, newdata = newdata, .args = pars)
      list(response = p)
    }
  )
)

.extralrns_dict$add("regr.gbm", LearnerRegrGBM)
