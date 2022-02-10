#' @title Regression Clusterwise Linear Regression Learner
#' @author ricardo-ams
#' @name mlr_learners_regr.clr
#'
#' @template class_learner
#' @templateVar id regr.clr
#' @templateVar caller clr
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerRegrClr = R6Class("LearnerRegrClr",
  inherit = LearnerRegr,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        K        = p_int(lower = 1L, default = 2L, tags = "train"),
        m        = p_dbl(lower = 1.0, default = 1.0, tags = "train"),
        nstart   = p_int(lower = 1L, default = 1L, tags = "train"),
        iter.max = p_int(lower = 1L, default = 1000L, tags = "train")
      )

      ps$values = list(
        K        = 2L,
        m        = 1.3,
        nstart   = 1L,
        iter.max = 1000L
      )

      super$initialize(
        id            = "regr.clr",
        packages      = "WCLR",
        feature_types = c("integer", "numeric"),
        predict_types = c("response"),
        param_set     = ps,
        man           = "mlr3extralearners::mlr_learners_regr.clr"
      )
    }
  ),

  private = list(

    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")

      # set column names to ensure consistency in fit and predict
      self$state$feature_names = task$feature_names

      # extract features from data
      X = task$data(cols = task$feature_names)

      # extract outcomes from data
      y = task$data(cols = task$target_names)[[1L]]

      mlr3misc::invoke(WCLR::clr,
                       X        = as.matrix(X),
                       y        = as.numeric(y),
                       K        = pars$K,
                       m        = pars$m,
                       nstart   = pars$nstart,
                       iter.max = pars$iter.max)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      # pars = self$param_set$get_values(tags = "predict")

      # get newdata and ensure same ordering in train and predict
      newdata = task$data(cols = self$state$feature_names)

      pred = mlr3misc::invoke(predict,
                              self$model,
                              newdata = newdata)
      list(response = pred)
    }
  )
)

.extralrns_dict$add("regr.clr", LearnerRegrClr)

