

#' Folds for [xgboost][xgboost::xgb.train]
#'
#' @description When calling [xgb.cv][xgboost::xgb.cv], analysts
#'   may wish to manually specify `folds` rather than `nfolds`.
#'   This function generates an object that can be given as
#'   an input for `folds` and additionally lets the analyst
#'   specify stratifying variables to create balanced folds.
#'
#' @param data a [data.frame] or [tibble][tibble::tibble-package] that
#'   will be used to generate folds.
#'
#' @param nfolds (integer) the number of folds
#'
#' @param strata A variable that is used to conduct stratified sampling
#'   to create the folds. This could be a single character value or a
#'   variable name that corresponds to a variable in `data`.
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(a=1:100, b=100:1, c=rep(letters[1:2],50))
#'
#' xgb_folds(data = df, nfolds = 10)
#' xgb_folds(data = df, nfolds = 100)
#' xgb_folds(data = df, nfolds = 10, strata = c)

xgb_folds <- function(data, nfolds, strata=NULL){

  if(vctrs::vec_size(data) < nfolds)
    stop("nfolds must be >= number of rows in data")

  strata <- tidyselect::vars_select(names(data), {{strata}})
  rsamp_folds <- rsample::vfold_cv(data, v=nfolds, strata = strata)
  purrr::map(rsamp_folds$splits, rsample::complement)

}
