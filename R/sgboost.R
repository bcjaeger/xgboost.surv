

#' Survival Gradient Booster
#'
#' @description The `sgb_fit` function is a wrapper for
#'  [xgboost][xgboost::xgb.train] designed to implement survival
#'  analyses.
#'
#' @param sgb_df An object of class 'sgb_data' (see [sgb_data]).
#'
#' @param eval_time_quants To evaluate risk prediction models, a set of
#'  evaluation times are created using the observed event times in `sgb_df`.
#'  These unique event times are truncated by including the times that are
#'  above and below and lower and upper quantiles of time specified in
#'  `eval_time_quants`, respectively. For example, To include all times,
#'  use `eval_time_quants = c(0,100)`. To include the times between the
#'  first and second quartiles, use `eval_time_quants = c(25, 50)`
#'
#' @inheritParams xgboost::xgb.train
#'
#' @return An `sgb_booster` object containing:
#'
#'   - `fit`: An `xgb.booster` object (see [xgboost][xgboost::xgb.train]).
#'
#'   - `label`: A numeric vector with time-to-event values, where
#'   censored observations have negative times and uncensored
#'   observations have positive times (see [sgb_label]).
#'
#'   - `predictions` Predicted values from `fit` for the training
#'   data. These predictions are saved as they are required to
#'   estimate the baseline hazard function of `fit`.
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
#'   nrounds = NULL,
#'   verbose = TRUE,
#'   print_every_n = 10
#' )
#'
#'

sgb_fit <- function(
  sgb_df,
  nrounds = NULL,
  eval_time_quants = c(0.10, 0.90),
  missing = NA,
  weight = NULL,
  params = sgb_params(),
  verbose = 1,
  print_every_n = max(c(1, round(nrounds/5))),
  early_stopping_rounds = NULL,
  maximize = NULL,
  save_period = NULL,
  save_name = "sgboost.model",
  xgb_model = NULL,
  callbacks = list()
) {

  .dots <- list(
    data = sgb_df$data,
    label = sgb_df$label,
    missing = missing,
    weight = weight,
    params = params,
    nrounds = nrounds,
    verbose = verbose,
    print_every_n = print_every_n,
    early_stopping_rounds = early_stopping_rounds,
    maximize = maximize,
    save_period = save_period,
    save_name = save_name,
    xgb_model = xgb_model,
    callbacks = callbacks
  )

  if(is.null(nrounds)){

    message("Applying cross-validation to determine nrounds")

    cv_args <- .dots
    cv_args$nrounds <- 5000
    cv_args$early_stopping_rounds <- early_stopping_rounds %||% 50
    cv_args$nfold <- 10
    xgb_cv = do.call(xgboost::xgb.cv, args = cv_args)

    .dots$nrounds <- xgb_cv$best_iteration
    .dots$early_stopping_rounds <- NULL

  }

  xgb_fit <- do.call(xgboost::xgboost, args = .dots)
  xgb_prd <- stats::predict(xgb_fit, newdata = .dots$data, outputmargin = TRUE)

  eval_time_boundaries <- get_time(sgb_df$label) %>%
    stats::quantile(probs = eval_time_quants)

  eval_times <- sgb_df$label[sgb_df$label > 0]
  eval_times <- eval_times[eval_times >= min(eval_time_boundaries)]
  eval_times <- eval_times[eval_times <= max(eval_time_boundaries)]
  eval_times <- unique(sort(eval_times, decreasing = FALSE))

  structure(
    .Data = list(
      fit = xgb_fit,
      label = .dots$label,
      eval_times = eval_times,
      training_predictions = xgb_prd
    ),
    class = 'sgb_booster'
  )

}

#' Create data for `sgboost`
#'
#' @description [xgboost][xgboost::xgb.train] operates using a data structure
#'   called `xgb.DMatrix`. The `xgboost` functions can internally create
#'   these data structures if they are given a matrix with columns giving
#'   predictor variables and a vector representing the label. For survival
#'   analysis, the label vector is a combination of time/status values
#'   (see [sgb_label]). This function automates the creation of a label
#'   vector and creates a list of components that are easily plugged in
#'   to `xgboost` functions.
#'
#' @param data the data containing predictor variables and a label column
#'
#' @param status a numeric vector indicating status at a given time.
#'  Normally, 0 indicates no event and 1 indicates an event occurred.
#'
#' @param time a numeric vector of follow-up time values.
#'
#' @param label a numeric vector based on `time` and `status`
#'   values. Time values should be less than zero for censored
#'   observations, and greater than zero for non-censored
#'   observations.
#'
#' @return an object of class `sgb_data` with components:
#'
#'  - `data`: a matrix with columns representing predictor variables
#'
#'  - `label`: a numeric vector representing time until event. Negative
#'    times indicate that an event did not occur, but the observation
#'    was censored at the absolute value of the given time. Positive times
#'    indicate the time of the event.
#'
#' @export
#'
#' @examples
#'
#' df = data.frame(time=c(1,2,3), status = c(0,0,1), x = c(2,2,1))
#'
#' as_sgb_data(df, status = status, time = time)
#'
#' sgb_data(df, label = sgb_label(df$time, df$status))
#'

sgb_data <- function(data, label){

  if( !all( purrr::map_lgl(data, is.numeric) ) ) stop(
    "all columns of data must be numeric. ",
    "Try using spread_cats()"
  )

  if( !tibble::is_tibble(data) ) data %<>% tibble::as_tibble()

  if( vctrs::vec_size(data) == 0 ) stop(
    "data is empty."
  )

  structure(
    .Data = list(
      data = as.matrix(data),
      label = label
    ),
    class = "sgb_data"
  )

}

#' @rdname sgb_data
#' @export
as_sgb_data <- function(data, status, time){

  if( !all( purrr::map_lgl(data, is.numeric) ) ) stop(
    "all columns of data must be numeric. ",
    "Try using spread_cats()"
  )

  str_status <- rlang::as_string(rlang::ensym(status))
  str_time <- rlang::as_string(rlang::ensym(time))

  indx_status <- which(names(data) == str_status)
  if(rlang::is_empty(indx_status)) stop(
    glue::glue("{str_status} not found in data"),
    call. = FALSE
  )

  indx_time <- which(names(data) == str_time)
  if(rlang::is_empty(indx_time)) stop(
    glue::glue("{str_time} not found in data"),
    call. = FALSE
  )

  if( !tibble::is_tibble(data) ) data %<>% tibble::as_tibble()

  if(ncol(data) < 3) stop(
    "data only contain time and status columns. ",
    "What about predictors?"
  )

  structure(
    .Data = list(
      data = as.matrix(data[, -c(indx_status, indx_time), drop=FALSE]),
      label = sgb_label(time = data[[str_time]], status = data[[str_status]])
    ),
    class = "sgb_data"
  )

}


#' Coerce `sgb_data` to Data Frame.
#'
#' @param x an `sgb_data` object
#' @param ... Currently not used
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#'
#' library(tibble)
#'
#' df = data.frame(time=c(1,2,3), status = c(0,0,1), x = c(2,2,1))
#'
#' df_sgb <- as_sgb_data(df, status = status, time = time)
#'
#' tbl_sgb <- as_tibble(df_sgb)
#'
#' tbl_sgb
#'

as_tibble.sgb_data <- function(x, ...){

  output <- tibble::as_tibble(x$data)
  output$status <- get_status(x$label)
  output$time <- get_time(x$label)

  dplyr::select(output, time, status, dplyr::everything())

}

is_sgb_data <- function(data){
  inherits(data, 'sgb_data')
}

is_sgb_booster <- function(x){
  inherits(x, 'sgb_booster')
}

#' Labels for [xgboost][xgboost::xgb.train]
#'
#' @description `xgboost` functions expect a certain type of label
#'  vector for certain problem types. For survival models, `xgboost`
#'  labels should be time-to-event values with negative times
#'  indicating censored observations and postive times indicating
#'  events.
#'
#' @param time a numeric vector with time-to-event values.
#' @param status a numeric vector of 0s and 1s, where 0 indicates
#'    censoring and 1 indicates that the event occurred.
#'
#' @examples
#'
#' sgb_label(
#'   time = c(1,2,3),
#'   status = c(1,0,1)
#' )
#'
#' @export

sgb_label <- function(time, status){

  # check outcomes

  time_okay <- time > 0

  if(!all(time_okay)){

    bad_indx <- which(!time_okay)

    if(length(bad_indx) > 5) bad_indx <- bad_indx[1:5]

    out_indx <- glue::glue_collapse(
      bad_indx,
      sep = ', ',
      last = ' and '
    )

    stop(
      glue::glue(
        "The time vector contains negative values. \\
        \nCheck the following indices in time: {out_indx}"
      ),
      call. = FALSE
    )

  }

  status_okay <- status %in% c(0,1)

  if(!all(status_okay)){

    bad_indx <- which(!status_okay)

    if(length(bad_indx) > 5) bad_indx <- bad_indx[1:5]

    out_indx <- glue::glue_collapse(
      bad_indx,
      sep = ', ',
      last = ' and '
    )

    stop(
      glue::glue(
        "All status values should be 0 or 1,\\
        and there should be no missing values. \\
        \nCheck the following indices in time: {out_indx}"
      ),
      call. = FALSE
    )

  }

  censor_indx <- status != 1
  time[censor_indx] <- (-1) * (time[censor_indx])
  time

}

#' Survival Parameters for [xgboost][xgboost::xgb.train]
#'
#' @param params a list of tuning parameters for an `xgboost` model.
#' @param ... name-value pairs (e.g., maxdepth = 2) of tuning parameters
#'   and tuning parameter values.
#'
#' @return a modified list with values for `objective` and `eval_metric`
#' @export
#'
#' @examples
#'
#' sgb_params()
#'
#' as_sgb_params(list(max_depth=1))
#'
as_sgb_params <- function(params){

  stopifnot(is.list(params))

  params$objective <- 'survival:cox'
  params$eval_metric <- 'cox-nloglik'

  params

}

#' @rdname as_sgb_params
#' @export
sgb_params <- function(...){

  .dots <- list(...)
  as_sgb_params(.dots)

}



get_status <- function(sgb_label){
  as.numeric(sgb_label > 0)
}

get_time <- function(sgb_label){
  abs(sgb_label)
}



