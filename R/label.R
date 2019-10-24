

#' Labels for [xgboost][xgboost::xgb.train]
#'
#' @description `xgboost` functions expect a certain type of label
#'  vector for certain problem types. For survival models, `xgboost`
#'  labels should be time-to-event values with negative times
#'  indicating censored observations and postive times indicating
#'  events. For binary classification problems, `xgboost` expects
#'  a vector of 0's and 1's with 1 indicating an event and 0 indicating
#'  no event. For K-class classification problems, `xgboost` expects
#'  a numeric vector with values of 0, 1, ..., and K-1. The set of
#'  functions described here work to make label creation a little
#'  less tedious.
#'
#' @param time a numeric vector with time-to-event values.
#' @param status a numeric vector of 0s and 1s, where 0 indicates
#'    censoring and 1 indicates that the event occurred.
#'
#' @note `xgboost` doesn't expect any special characteristics for
#'   continous outcomes, so we did not develop a function to
#'   handle them.
#'
#' @examples
#'
#' lbl_survival(
#'   time = c(1,2,3),
#'   status = c(1,0,1)
#' )
#'
#' lbl_binary(
#'   response = factor(x = c("A","A","B"))
#' )
#'
#' lbl_multi(
#'   response = factor(x = c("A","C","B"))
#' )
#'
#' @export

lbl_survival <- function(time, status){

  # check outcomes

  time_okay <- all(time > 0)

  if(!time_okay){

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
        Check the following indices in time: {out_indx}"
      ),
      call. = FALSE
    )

  }

  status_okay <- all(status %in% c(0,1))

  if(!status_okay){

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
        Check the following indices in time: {out_indx}"
      ),
      call. = FALSE
    )

  }

  censor_indx <- status != 1
  time[censor_indx] <- (-1) * (time[censor_indx])
  time

}

#' @rdname lbl_survival
#' @export
lbl_multi <- function(response){

  if (is.logical(response)){

    stop("logical vector supplied for multi-class classification.")

  }

  if(is.numeric(response)){

    if(length(unique(response)) == 2){
      stop("multi-class labels must have >2 unique values")
    }

    return(response)

  }

  if(is.factor(response)){

    if(length(levels(response)) == 2){
      stop("multi-class labels must have >2 unique categories")
    }

    return(as.numeric(response) - 1)

  }

}

#' @rdname lbl_survival
#' @export
lbl_binary <- function(response){

  if(is.numeric(response)){

    if(!all(unique(response)) %in% c(0,1)){
      stop(
        "numeric binary labels should have values of 0 and 1",
        call. = FALSE
      )
    }

    output <- response

  } else if(is.factor(response)){

    if(length(levels(response)) > 2){
      stop("input has more than 2 categories")
    }

    output <- as.numeric(response) - 1

  } else if (is.logical(response)){

    output <- as.numeric(response)

  }

  output

}


get_status <- function(surv_label){
  as.numeric(surv_label > 0)
}

get_time <- function(surv_label){
  abs(surv_label)
}
