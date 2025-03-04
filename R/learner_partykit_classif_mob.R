#' @title Classification Model-based Recursive Partitioning Learner
#' @author sumny
#' @name mlr_learners_classif.mob
#'
#' @template class_learner
#' @templateVar id classif.mob
#' @templateVar caller mob
#'
#' @references
#' Hothorn T, Zeileis A (2015).
#' “partykit: A Modular Toolkit for Recursive Partytioning in R.”
#' Journal of Machine Learning Research, 16(118), 3905-3909.
#' \url{http://jmlr.org/papers/v16/hothorn15a.html}
#'
#' Hothorn T, Hornik K, Zeileis A (2006).
#' “Unbiased Recursive Partitioning: A Conditional Inference Framework.”
#' Journal of Computational and Graphical Statistics, 15(3), 651–674.
#' \doi{10.1198/106186006x133933}
#'
#' Zeileis A, Hothorn T, Hornik K (2008).
#' “Model-Based Recursive Partitioning.”
#' Journal of Computational and Graphical Statistics, 17(2), 492–514.
#' \doi{10.1198/106186008X319331}
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifMob = R6Class("LearnerClassifMob", inherit = LearnerClassif,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        # missing: subset, na.action, weights (see bottom)
        rhs = p_uty(custom_check = check_character,
          tags = "train"),
        fit = p_uty(custom_check = function(x) {
          check_function(x,
            args = c("y", "x", "start", "weights", "offset", "..."))
        }, tags = "train"),
        offset = p_uty(tags = "train"),
        cluster = p_uty(tags = "train"),
        # all in mob_control()
        alpha = p_dbl(default = 0.05, lower = 0, upper = 1,
          tags = "train"),
        bonferroni = p_lgl(default = TRUE, tags = "train"),
        # minsize, minsplit, minbucket are equivalent, adaptive default
        minsize = p_int(lower = 1L, tags = "train"),
        minsplit = p_int(lower = 1L, tags = "train"),
        minbucket = p_int(lower = 1L, tags = "train"),
        maxvar = p_int(lower = 1L, tags = "train"),
        maxdepth = p_int(default = Inf, lower = 0L,
          special_vals = list(Inf), tags = "train"),
        mtry = p_int(default = Inf, lower = 0L,
          special_vals = list(Inf), tags = "train"),
        trim = p_dbl(default = 0.1, lower = 0, tags = "train"),
        breakties = p_lgl(default = FALSE, tags = "train"),
        parm = p_uty(tags = "train"),
        dfsplit = p_int(lower = 0L, tags = "train"),
        prune = p_uty(tags = "train"),
        restart = p_lgl(default = TRUE, tags = "train"),
        verbose = p_lgl(default = FALSE, tags = "train"),
        caseweights = p_lgl(default = TRUE, tags = "train"),
        ytype = p_fct(default = "vector",
          levels = c("vector", "matrix", "data.frame"), tags = "train"),
        xtype = p_fct(default = "matrix",
          levels = c("vector", "matrix", "data.frame"), tags = "train"),
        terminal = p_uty(default = "object", tags = "train"),
        inner = p_uty(default = "object", tags = "train"),
        model = p_lgl(default = TRUE, tags = "train"),
        numsplit = p_fct(default = "left", levels = c("left", "center"),
          tags = "train"),
        catsplit = p_fct(default = "binary",
          levels = c("binary", "multiway"), tags = "train"),
        vcov = p_fct(default = "opg",
          levels = c("opg", "info", "sandwich"), tags = "train"),
        ordinal = p_fct(default = "chisq",
          levels = c("chisq", "max", "L2"), tags = "train"),
        nrep = p_int(default = 10000, lower = 0L, tags = "train"),
        applyfun = p_uty(tags = "train"),
        cores = p_int(default = NULL, special_vals = list(NULL),
          tags = "train"),
        # additional arguments passed to fitting function
        additional = p_uty(custom_check = check_list,
          tags = "train"),
        # the predict function depends on the predict method of the fitting
        # function itself and can be passed via type, see predict.modelparty
        # most fitting functions should not need anything else than the model
        # itself, the newdata, the original task and a
        # predict type
        predict_fun = p_uty(custom_check = function(x) {
          check_function(x,
            args = c("object", "newdata", "task", ".type"))
        }, tags = "predict")
      )

      ps$add_dep("nrep", on = "ordinal", cond = CondEqual$new("L2"))

      super$initialize(
        id = "classif.mob",
        param_set = ps,
        # predict, features and properties depend on the fitting function itself
        predict_types = c("response", "prob"),
        feature_types = c("logical", "integer", "numeric", "character",
          "factor", "ordered"),
        properties = c("weights", "twoclass", "multiclass"),
        packages = c("mlr3extralearners", "partykit", "sandwich", "coin"),
        man = "mlr3extralearners::mlr_learners_classif.mob"
      )
    }
  ),

  private = list(
    .train = function(task) {

      # FIXME: check if rhs variables are present in data?
      formula = task$formula(self$param_set$values$rhs)
      pars = self$param_set$get_values(tags = "train")
      pars_control = pars[which(names(pars) %in%
                                  methods::formalArgs(partykit::mob_control))]
      pars_additional = self$param_set$values$additional
      pars = pars[names(pars) %nin%
        c("rhs", names(pars_control), "additional")]
      control = mlr3misc::invoke(partykit::mob_control, .args = pars_control)
      if ("weights" %in% task$properties) { # weights are handled here
        pars = mlr3misc::insert_named(pars, list(weights = task$weights$weight))
      }
      # append the additional parameters to be passed to the fitting function
      pars = append(pars, pars_additional)

      # FIXME: contrasts?
      mlr3misc::invoke(partykit::mob,
        formula = formula,
        data = task$data(),
        control = control,
        .args = pars
      )
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      # type is the type argument passed to predict.modelparty
      # (actually a predict function used to compute the predictions as we want)
      # .type is then the actual predict type as set for the learner
      preds = mlr3misc::invoke(predict, object = self$model, newdata = newdata,
        type = self$param_set$values$predict_fun, task = task,
        .type = self$predict_type)
      if (self$predict_type == "response") {
        list(response = preds)
      } else {
        list(prob = preds)
      }
    }
  )
)

.extralrns_dict$add("classif.mob", LearnerClassifMob)
