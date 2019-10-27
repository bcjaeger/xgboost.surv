

#eval_cstats
#eval_calibr

#' Integrated Brier Score
#'
#' @param surv_probs a matrix of survival probabilities
#' @param label a numeric vector of class `sgb_label`
#' @param time (needed iff `label` is unspecified) a numeric vector of time values.
#'   All `time` values must be greater than 0.
#' @param status (needed iff `label` is unspecified) a numeric vector of
#'   status values. All `status` values should be 0 or 1, with 1 indicating
#'   that the event of interest occurred and 0 indicating censoring.
#' @param cens_data (optional) a `data.frame` or `tibble` containing a
#'   selection of variables that will be used to model censoring probability.
#'   The inverse probability of censoring will then be used to weight each
#'   observation's contribution go the integrated Brier score.
#' @param cens_model Method for estimating inverse probability of
#' censoring weights:
#'
#' - `cox`: A semi-parametric Cox proportional hazard model is fitted
#'   to the censoring times
#'
#' - `nonpar`: Nonparametric extension of the Kaplan-Meier for the censoring
#'   times using symmetric nearest neighborhoods – available for arbitrary
#'   many strata variables on the right hand side of argument formula
#'   but at most one continuous variable (see [prodlim][prodlim::prodlim]
#'   and [neighborhood][prodlim::neighborhood])
#'
#' - `aalen`: The nonparametric Aalen additive model fitted to the
#'   censoring times. Requires the timereg package.
#'
#' **Note**: if `cens_data` is unspecified, the Kaplan-Meier estimator
#' will be used to estimate inverse probability of censoring weights
#'
#' @param eval_times A numeric vector giving times at which to compute
#'   Brier scores, which will then be used collectively to compute the
#'   integrated Brier score.
#' @param scale (`TRUE`/`FALSE`) if `TRUE`, a marginal Kaplan-Meier
#'   prediction model will be fitted to the given (`time`, `status`)
#'   values and used as a reference Brier score. Then, a scaled Brier
#'   score will be computed as 1 - (model-IBS / reference-IBS)
#'
#' @return a numeric value. If `scale = TRUE`, return values of
#'   0 indicate a non-informative model and values of 1 indicate
#'   a perfect model. If `scale = FALSE`, return values of 0 indicate
#'   a perfect model, and return values greater than 0 are difficult
#'   to interpret.
#' @export
#'
#' @examples
#'
#' # prodlim needs to be loaded for pec functions to work.
#' library(prodlim)
#' library(survival)
#'
#'set.seed(329)
#' # predict 1/2 for everyone, all the time
#' surv_probs <- matrix(
#'   data = rep(1/2, 1000),
#'   nrow = 100,
#'   ncol = 10
#' )
#'
#' # make random time & status values
#' time = runif(100, min = 1, max = 10)
#' status = c(rep(1, 50), rep(0, 50))
#'
#' # use all the possible times
#' eval_times = seq(1, 10, length.out = 10)
#'
#' # censor data has one variable that is
#' # related to time values
#' cens_data = data.frame(x1 = time + rnorm(100, sd = 1/2))
#'
#' # note that the Brier score is 1/4, as expected, when
#' # no adjustment is applied for censoring.
#' eval_bscore(
#'   surv_probs = surv_probs,
#'   label = sgb_label(time, status),
#'   eval_times = eval_times,
#'   cens_data = cens_data,
#'   cens_model = 'marginal',
#'   scale = FALSE
#' )
#'
#' # Now adjust for censoring:
#' eval_bscore(
#'   surv_probs = surv_probs,
#'   label = sgb_label(time, status),
#'   eval_times = eval_times,
#'   cens_data = cens_data,
#'   cens_model = 'cox',
#'   scale = FALSE
#' )

eval_bscore <- function(
  surv_probs,
  label = NULL,
  time = NULL,
  status = NULL,
  cens_data = NULL,
  cens_model = 'cox',
  eval_times = NULL,
  scale = TRUE
) {

  if(!is.null(time) && !is.null(status)){
    if(length(time) != length(status)){
      stop("length of time and status should be equal", call. = FALSE)
    }
  }

  label <- label %||% sgb_label(time, status)

  eval_times = eval_times %||% attr(surv_probs, 'eval_times')

  if(is.null(eval_times)) stop(
    "eval_times must be specified",
    call. = FALSE
  )

  if(length(eval_times) != ncol(surv_probs)) stop(
    "length(eval_times) is not equal to ncol(surv_probs)",
    call. = FALSE
  )

  new_data <- tibble(
    time = get_time(label),
    status = as.integer(get_status(label))
  )

  if(!is.null(cens_data)){

    # check length
    if(nrow(cens_data) != nrow(new_data)) stop(
      "nrow(cens_data) is not equal to length(time)"
    )

    xnames <- paste(names(cens_data), collapse = ' + ')

    new_data %<>% dplyr::bind_cols(cens_data)
    formula <- glue::glue("Surv(time, status) ~ {xnames}")

  } else {

    formula <- "Surv(time, status) ~ 1"

  }

  suppressMessages(
    expr = {
      bscores <- pec::pec(
        object = surv_probs,
        cens.model = cens_model,
        formula = stats::as.formula(formula),
        data = new_data,
        times = eval_times,
        exact = FALSE,
        start = eval_times[1],
        maxtime = eval_times[length(eval_times)]
      ) %>%
        pec::ibs()
    }
  )

  if(scale){
    1 - bscores[-1, ] / bscores[1, ]
  } else {
    bscores[-1, ]
  }

}

