#' @title Density Logspline Learner
#' @author RaphaelS1
#' @name mlr_learners_dens.logspline
#' @template class_learner
#' @templateVar id dens.logspline
#' @templateVar caller logspline
#'
#' @references
#' Charles Kooperberg and Charles J. Stone.
#' Logspline density estimation for censored data (1992).
#' Journal of Computational and Graphical Statistics, 1, 301–328.
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerDensLogspline = R6Class("LearnerDensLogspline",
  inherit = mlr3proba::LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
          lbound = p_dbl(tags = "train"),
          ubound = p_dbl(tags = "train"),
          maxknots = p_dbl(default = 0, lower = 0, tags = "train"),
          knots = p_uty(tags = "train"),
          nknots = p_dbl(default = 0, lower = 0, tags = "train"),
          penalty = p_uty(tags = "train"),
          silent = p_lgl(default = TRUE, tags = "train"),
          mind = p_dbl(default = -1, tags = "train"),
          error.action = p_int(default = 2, lower = 0, upper = 2, tags = "train")
      )


      super$initialize(
        id = "dens.logspline",
        packages = c("mlr3extralearners", "logspline"),
        feature_types = c("integer", "numeric"),
        predict_types = c("pdf", "cdf"),
        param_set = ps,
        man = "mlr3extralearners::mlr_learners_dens.logspline"
      )
    }
  ),

  private = list(
    .train = function(task) {

      data = task$data()[[1]]

      pars = self$param_set$get_values(tags = "train")

      fit = mlr3misc::invoke(logspline::logspline, x = data, .args = pars)

      pdf = function(x) {} #nolint
      body(pdf) = substitute({
        mlr3misc::invoke(logspline::dlogspline, q = x, fit = fit)
      })

      cdf = function(x) {} #nolint
      body(cdf) = substitute({
        mlr3misc::invoke(logspline::plogspline, q = x, fit = fit)
      })

      quantile = function(p) {} #nolint
      body(quantile) = substitute({
        mlr3misc::invoke(logspline::qlogspline, p = p, fit = fit)
      })

      rand = function(n) {} #nolint
      body(rand) = substitute({
        mlr3misc::invoke(logspline::rlogspline, n = n, fit = fit)
      })

      distr6::Distribution$new(
        name = "Logspline Density Estimator",
        short_name = "LogsplineDens",
        pdf = pdf, cdf = cdf, quantile = quantile, rand = rand, type = set6::Reals$new())

    },

    .predict = function(task) {
      newdata = task$data()[[1]]
      list(pdf = self$model$pdf(newdata), cdf = self$model$cdf(newdata))
    }
  )
)

.extralrns_dict$add("dens.logspline", LearnerDensLogspline)
