
#' Baseline Hazard
#'
#' @description This function is a wrapper for the
#'   [gbm::basehaz.gbm()]. The function
#'   computes the Breslow estimator of the baseline
#'   hazard function for a proportional hazard
#'   regression model.
#'
#' @param sgb_booster an object of class `sgb_booster` (see [sgb_fit]).
#'
#' @param eval_times Values at which the baseline hazard will
#'   be evaluated.
#'
#' @param smooth If `TRUE` `sgb_bhaz` will smooth the estimated
#'   baseline hazard using Friedman's super smoother
#'   [supsmu][stats::supsmu()].
#'
#' @param cumulative If `TRUE` the cumulative survival function
#'   will be computed.
#'
#' @return A vector with a baseline hazard value corresponding to each
#'   time in `eval_times`  If cumulative is `TRUE`, the returned vector
#'   evaluates the cumulative hazard function at those values.
#'
#' @export
#'

sgb_bhaz <- function(
  sgb_booster,
  eval_times = NULL,
  smooth = FALSE,
  cumulative = TRUE
){


  eval_times_okay <- all(diff(eval_times)>0)

  stopifnot(is.logical(smooth))
  stopifnot(is.logical(cumulative))
  stopifnot(is_sgb_booster(sgb_booster))

  if(!eval_times_okay){
    stop(
      "eval_times should be a monotonically increasing sequence", call.=FALSE
    )
  }

  gbm::basehaz.gbm(
    t = get_time(sgb_booster$label),
    delta = get_status(sgb_booster$label),
    f.x = sgb_booster$training_predictions,
    t.eval = eval_times,
    smooth = smooth,
    cumulative = cumulative
  )

}


#' Boosting Predictions
#'
#' @param object a `sgb_booster` object
#' @param new_data data to compute predictions for.
#' @param eval_times numeric vector of times to compute survival probabilities.
#' @param ... Additional arguments passed to other functions.
#'   - **smooth:** if `TRUE`, smooth the estimated baseline hazard
#'   using Friedman's super smoother [supsmu][stats::supsmu].
#'   - **ntreelimit:** limit the number of model's trees or boosting
#'   iterations used in prediction. If unspecified, all trees will be used.
#'
#' @return a `matrix` with number of columns equal to the number of
#'  `eval_times` and number of rows equal to the number of rows in
#'  `new_data`. Additionally, `eval_times` are attached to the output
#'  as an attribute.
#' @export
#'
#' @examples
#'
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#' s  <- as.numeric(x1 + x2 + rnorm(100) > 0)
#' t  <- runif(100, min=1, max=10)
#'
#' df = data.frame(time=t, status=s, x1=x1, x2=x2)
#'
#' df = as_sgb_data(df, time=time, status=status)
#'
#' sgb_booster <- sgb_fit(
#'   sgb_df = df,
#'   params = sgb_params(max_depth=1),
#'   nrounds = 10,
#'   verbose = FALSE
#' )
#'
#' sgb_probs <- predict(sgb_booster, new_data = df)
#'
#'
predict.sgb_booster <- function(object, new_data, eval_times = NULL, ...){

  .dots <- list(...) %>%
    check_dots(valid_args = c('smooth', 'ntreelimit'))

  smooth <- if('smooth' %in% names(.dots))
    .dots$smooth
  else
    FALSE

  ntreelimit <- if('ntreelimit' %in% names(.dots))
    .dots$ntreelimit
  else
    NULL

  eval_times <- eval_times %||% object$eval_times

  base_haz <- sgb_bhaz(
    sgb_booster = object,
    eval_times = eval_times,
    smooth = smooth,
    cumulative = TRUE
  )

  predictions <- stats::predict(
    object = object$fit,
    newdata = if(is_sgb_data(new_data)) new_data$data else new_data,
    outputmargin = TRUE,
    ntreelimit = ntreelimit
  )

  prb <- matrix(
    data = NA_real_,
    nrow = length(predictions),
    ncol = length(eval_times)
  )

  for( i in seq_along(base_haz) ){
    prb[,i] <- exp(-exp(predictions) * (base_haz[i]))
  }

  attr(prb, 'eval_times') <- eval_times
  #class(prb) <- c('sgb_probs', 'matrix')

  prb

}
