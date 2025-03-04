% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learner_survivalmodels_surv_coxtime.R
\name{mlr_learners_surv.coxtime}
\alias{mlr_learners_surv.coxtime}
\alias{LearnerSurvCoxtime}
\title{Survival Cox-Time Learner}
\description{
Calls \link[survivalmodels:coxtime]{survivalmodels::coxtime} from package \CRANpkg{survivalmodels}.
}
\section{Dictionary}{
 This \link{Learner} can be instantiated via the
\link[mlr3misc:Dictionary]{dictionary} \link{mlr_learners} or with the associated
sugar function \code{\link[=lrn]{lrn()}}:\preformatted{mlr_learners$get("surv.coxtime")
lrn("surv.coxtime")
}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{surv}
\item Predict Types: \dQuote{crank}, \dQuote{distr}
\item Feature Types: \dQuote{integer}, \dQuote{numeric}
\item Required Packages: \CRANpkg{mlr3extralearners}, \CRANpkg{survivalmodels}, \CRANpkg{distr6}, \CRANpkg{reticulate}
}
}

\section{Parameters}{
\tabular{lllll}{
   Id \tab Type \tab Default \tab Levels \tab Range \cr
   frac \tab numeric \tab 0 \tab  \tab \eqn{[0, 1]}{[0, 1]} \cr
   standardize_time \tab logical \tab FALSE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   log_duration \tab logical \tab FALSE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   with_mean \tab logical \tab TRUE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   with_std \tab logical \tab TRUE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   num_nodes \tab list \tab 32, 32 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   batch_norm \tab logical \tab TRUE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   dropout \tab numeric \tab - \tab  \tab \eqn{[0, 1]}{[0, 1]} \cr
   activation \tab character \tab relu \tab celu, elu, gelu, glu, hardshrink, hardsigmoid, hardswish, hardtanh, relu6, leakyrelu, \link{...} \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   device \tab list \tab - \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   shrink \tab numeric \tab 0 \tab  \tab \eqn{[0, \infty)}{[0, Inf)} \cr
   optimizer \tab character \tab adam \tab adadelta, adagrad, adam, adamax, adamw, asgd, rmsprop, rprop, sgd, sparse_adam \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   rho \tab numeric \tab 0.9 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   eps \tab numeric \tab 1e-08 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   lr \tab numeric \tab 1 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   weight_decay \tab numeric \tab 0 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   learning_rate \tab numeric \tab 0.01 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   lr_decay \tab numeric \tab 0 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   betas \tab list \tab 0.900, 0.999 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   amsgrad \tab logical \tab FALSE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   lambd \tab numeric \tab 1e-04 \tab  \tab \eqn{[0, \infty)}{[0, Inf)} \cr
   alpha \tab numeric \tab 0.75 \tab  \tab \eqn{[0, \infty)}{[0, Inf)} \cr
   t0 \tab numeric \tab 1e+06 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   momentum \tab numeric \tab 0 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   centered \tab logical \tab TRUE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   etas \tab list \tab 0.5, 1.2 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   step_sizes \tab list \tab 1e-06, 5e+01 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   dampening \tab numeric \tab 0 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   nesterov \tab logical \tab FALSE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   batch_size \tab integer \tab 256 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   epochs \tab integer \tab 1 \tab  \tab \eqn{[1, \infty)}{[1, Inf)} \cr
   verbose \tab logical \tab TRUE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   num_workers \tab integer \tab 0 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   shuffle \tab logical \tab TRUE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   best_weights \tab logical \tab FALSE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   early_stopping \tab logical \tab FALSE \tab TRUE, FALSE \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   min_delta \tab numeric \tab 0 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
   patience \tab integer \tab 10 \tab  \tab \eqn{(-\infty, \infty)}{(-Inf, Inf)} \cr
}
}

\examples{
if (requireNamespace("survivalmodels", quietly = TRUE) && requireNamespace("distr6", quietly = TRUE) && requireNamespace("reticulate", quietly = TRUE)) {
  learner = mlr3::lrn("surv.coxtime")
  print(learner)

  # available parameters:
  learner$param_set$ids()
}
}
\references{
Kvamme, H., Borgan, Ø., & Scheel, I. (2019).
Time-to-event prediction with neural networks and Cox regression.
Journal of Machine Learning Research, 20(129), 1–30.
}
\seealso{
\itemize{
\item \link[mlr3misc:Dictionary]{Dictionary} of \link[mlr3:Learner]{Learners}: \link[mlr3:mlr_learners]{mlr3::mlr_learners}.
\item \code{as.data.table(mlr_learners)} for a table of available \link[=Learner]{Learners} in the running session (depending on the loaded packages).
\item Chapter in the \href{https://mlr3book.mlr-org.com/}{mlr3book}: \url{https://mlr3book.mlr-org.com/basics.html#learners}
\item \CRANpkg{mlr3learners} for a selection of recommended learners.
\item \CRANpkg{mlr3cluster} for unsupervised clustering learners.
\item \CRANpkg{mlr3pipelines} to combine learners with pre- and postprocessing steps.
\item \CRANpkg{mlr3tuning} for tuning of hyperparameters, \CRANpkg{mlr3tuningspaces} for established default tuning spaces.
}
}
\author{
RaphaelS1
}
\section{Super classes}{
\code{\link[mlr3:Learner]{mlr3::Learner}} -> \code{\link[mlr3proba:LearnerSurv]{mlr3proba::LearnerSurv}} -> \code{LearnerSurvCoxtime}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{LearnerSurvCoxtime$new()}}
\item \href{#method-clone}{\code{LearnerSurvCoxtime$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="base_learner">}\href{../../mlr3/html/Learner.html#method-base_learner}{\code{mlr3::Learner$base_learner()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="format">}\href{../../mlr3/html/Learner.html#method-format}{\code{mlr3::Learner$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="help">}\href{../../mlr3/html/Learner.html#method-help}{\code{mlr3::Learner$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict">}\href{../../mlr3/html/Learner.html#method-predict}{\code{mlr3::Learner$predict()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict_newdata">}\href{../../mlr3/html/Learner.html#method-predict_newdata}{\code{mlr3::Learner$predict_newdata()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="print">}\href{../../mlr3/html/Learner.html#method-print}{\code{mlr3::Learner$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="reset">}\href{../../mlr3/html/Learner.html#method-reset}{\code{mlr3::Learner$reset()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="train">}\href{../../mlr3/html/Learner.html#method-train}{\code{mlr3::Learner$train()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvCoxtime$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvCoxtime$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
