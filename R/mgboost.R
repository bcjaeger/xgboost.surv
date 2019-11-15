#'
#'
#'
#' #' (ipa) Cross-validation for xgboosters
#' #'
#' #' @description This function is a wrapper for the
#' #'   [xgboost::xgb.cv()] function that automates some
#' #'   adjustments that should be applied when using `ipa` data.
#' #'   For example, the `subsample` tuning parameter is automatically
#' #'   adjusted based on the number of imputed datasets for `ipa` data.
#' #'   Additionally, the folds argument is adjusted such that
#' #'   observations in `ipa` data belonging to the same ID are
#' #'   allocated to the same data-fold.
#' #'
#' #' @details To ensure valid data-folds, the `folds` argument must be
#' #'   supplied. If `nfolds` is supplied instead, an error will occur.
#' #'
#' #' @inheritParams xgboost::xgb.cv
#' #'
#' #' @return a modified version of the output from
#' #'   [xgboost::xgb.cv()]. Specifically,
#' #'   an additional item, `compute_time` is included
#' #'   in the output list.
#' #'
#' #' @export
#'
#' # .dots <- list(
#' #   data = train$knn_lst,
#' #   label = label$train,
#' #   params = params,
#' #   verbose = 1,
#' #   print_every_n = 50,
#' #   early_stopping_rounds = 100,
#' #   folds = folds,
#' #   nrounds = 5000
#' # )
#' # miss_strat = 'stack'
#' # n_impute = length(nb_seq)
#'
#'
#' mgb_cv <- function(..., miss_strat, n_impute = NULL){
#'
#'   check_miss_strat(miss_strat = miss_strat)
#'   if(miss_strat == 'si') n_impute <- 1
#'
#'
#'   .dots <- list(...) %>%
#'     check_dots(
#'       valid_args = c(
#'         "params",
#'         "data",
#'         "nrounds",
#'         "label",
#'         "missing",
#'         "prediction",
#'         "showsd",
#'         "metrics",
#'         "obj",
#'         "feval",
#'         "folds",
#'         "verbose",
#'         "print_every_n",
#'         "early_stopping_rounds",
#'         "maximize",
#'         "callbacks"
#'       )
#'     )
#'
#'   if( !('data' %in% names(.dots)) ) {
#'     stop('data needs to be specified', call. = FALSE)
#'   }
#'
#'   if( !('params' %in% names(.dots)) ){
#'     stop('params needs to be specified', call. = FALSE)
#'   }
#'
#'   if ( !('folds' %in% names(.dots)) ) {
#'     stop("folds must be specified", call. = FALSE)
#'   }
#'
#'   if('nfold' %in% names(.dots)){
#'     stop(
#'       "To ensure valid comparisons, please specify only the folds argument",
#'       call. = FALSE
#'     )
#'   }
#'
#'   if(miss_strat == 'stacked'){
#'
#'     .dots %<>% check_xgb_stack_args(n_impute = n_impute)
#'
#'   }
#'
#'   if(miss_strat == 'mi'){
#'
#'     data_list <- prep_data_list(data=.dots$data, n_impute = n_impute)
#'     .dots$data <- NULL
#'     output <- mgb_cv_mi(data_list, args=.dots)
#'
#'   } else {
#'
#'     start <- Sys.time()
#'     output <- do.call(xgb.cv, args = .dots)
#'     stop <- Sys.time()
#'
#'     output %<>% mgb_cmp_time(start = start, stop = stop, args = .dots)
#'
#'   }
#'
#'   output
#'
#' }
#'
#' mgb_cmp_time <- function(object, start, stop, args){
#'
#'   if('early_stopping_rounds' %in% names(args)){
#'     divby <- object$best_iteration + args$early_stopping_rounds
#'   } else {
#'     divby <- args$nrounds
#'   }
#'
#'   object$compute_time <- list(
#'     overall = stop-start,
#'     by_iter = as.numeric(stop-start) / divby
#'   )
#'
#'   object
#'
#' }
#'
#' mgb_cv_mi <- function(data_list, args){
#'
#'   map(
#'     .x = data_list,
#'     .f = function(data){
#'
#'       args$data <- data
#'
#'       start <- Sys.time()
#'       output = do.call(xgb.cv, args)
#'       stop <- Sys.time()
#'
#'       output %<>% mgb_cmp_time(start = start, stop = stop, args = args)
#'       output
#'
#'     }
#'   )
#'
#' }
#'
#'
#' #' @export
#' mgb_cv_train <- function(
#'   data,
#'   label,
#'   folds,
#'   params,
#'   n_impute,
#'   miss_strat,
#'   nrounds = 1000,
#'   maximize = NULL,
#'   print_every_n = 100,
#'   early_stopping_rounds = 100
#' ) {
#'
#'   cv_args <- list(
#'     data = data,
#'     label = label,
#'     folds = folds,
#'     params = params,
#'     nrounds = nrounds,
#'     maximize = maximize,
#'     n_impute = n_impute,
#'     miss_strat = miss_strat,
#'     print_every_n = print_every_n,
#'     early_stopping_rounds = early_stopping_rounds
#'   )
#'
#'   cv_object <- do.call(mgb_cv, cv_args)
#'
#'   nrounds <-
#'     if(miss_strat == 'mi'){
#'       map_int(cv_object, 'best_iteration')
#'     } else {
#'       cv_object$best_iteration
#'     }
#'
#'   train_args <- list(
#'     data = data,
#'     label = label,
#'     params = params,
#'     nrounds = nrounds,
#'     maximize = maximize,
#'     n_impute = n_impute,
#'     miss_strat = miss_strat,
#'     print_every_n = print_every_n
#'   )
#'
#'   train_object <- do.call(mgb_train, train_args)
#'   train_object$cv_compute_time <- cv_object$compute_time
#'   train_object
#'
#' }
#'
#' #' (ipa) training for xgboosters
#' #'
#' #' @description This function is a wrapper for the
#' #'   [xgboost::xgb.train()] function that automates some
#' #'   adjustments that should be applied when using `ipa` data.
#' #'   Specifically, the `subsample` tuning parameter is automatically
#' #'   adjusted based on the number of imputed datasets for `ipa` data.
#' #'
#' #' @details When a list of multiply imputed data are supplied,
#' #'   each dataset will be used to train one xgboost model, separately.
#' #'   When a `ipa` dataset is supplied, only one xgboost model is
#' #'   trained, and the stacked data are subsampled such that each
#' #'   boosting step utilizes N / n_impute, where N is the number of
#' #'   rows in the stacked `ipa` data.
#' #'
#' #' @inheritParams xgboost::xgb.train
#' #'
#' #' @return a modified version of the output from
#' #'   [xgboost::xgb.cv()]. Specifically,
#' #'   an additional item, `compute_time` is included
#' #'   in the output list.
#' #'
#' #' @export
#' #'
#'
#' # .dots <- list(
#' #   data = tmp$training[[2]],
#' #   params = params,
#' #   verbose = 1,
#' #   print_every_n = 50,
#' #   folds = folds,
#' #   nrounds = map(tmp$cv_obj[[2]],'best_iteration')
#' # )
#'
#' mgb_train <- function(..., miss_strat, n_impute = NULL){
#'
#'   check_miss_strat(miss_strat = miss_strat)
#'   if(miss_strat == 'si') n_impute <- 1
#'
#'   .dots <- list(...) %>%
#'     check_dots(
#'       valid_args = c(
#'         "data",
#'         "label",
#'         "missing",
#'         "weight",
#'         "params",
#'         "nrounds",
#'         "verbose",
#'         "print_every_n",
#'         "early_stopping_rounds",
#'         "maximize",
#'         "save_period",
#'         "save_name",
#'         "xgb_model",
#'         "callbacks"
#'       )
#'     )
#'
#'   orig_label <- .dots$label
#'   n_obs <- nrow(.dots$data) / n_impute
#'
#'   if( !('data' %in% names(.dots)) ){
#'     stop('data needs to be specified', call. = FALSE)
#'   }
#'
#'   if( !('params' %in% names(.dots)) ){
#'     stop('params needs to be specified', call. = FALSE)
#'   }
#'
#'   if(miss_strat == 'stacked'){
#'
#'     .dots %<>% check_xgb_stack_args(n_impute = n_impute)
#'
#'   }
#'
#'
#'   if(miss_strat == 'mi'){
#'
#'     data_list <- prep_data_list(data=.dots$data, n_impute = n_impute)
#'
#'     if(length(.dots$nrounds) == n_impute){
#'       nround_list <- .dots$nrounds
#'     } else if(length(.dots$nrounds) == 1){
#'       nround_list <- rep(.dots$nrounds, n_impute)
#'     } else {
#'       stop(
#'         "nround should be length 1 or n_impute",
#'         call. = FALSE
#'       )
#'     }
#'
#'     .dots$data <- NULL
#'     .dots$nrounds <- NULL
#'
#'     output <- list(
#'       fit = mgb_train_mi(data_list, nround_list, args=.dots)
#'     )
#'
#'     output$train_predictions <- map2(
#'       .x = output$fit,
#'       .y = data_list,
#'       .f = ~ predict(
#'         object = .x,
#'         newdata = .y,
#'         outputmargin = TRUE
#'       )
#'     ) %>%
#'       reduce(`+`) %>%
#'       divide_by(n_impute) %>%
#'       as.numeric()
#'
#'     output$train_label <- orig_label
#'
#'
#'   } else {
#'
#'     start <- Sys.time()
#'     output <- list(fit = do.call(xgboost, args = .dots))
#'     stop <- Sys.time()
#'
#'     output$fit %<>% mgb_cmp_time(start = start, stop = stop, args = .dots)
#'
#'     output$train_predictions <- predict(
#'       object = output$fit,
#'       newdata = .dots$data,
#'       outputmargin = TRUE
#'     ) %>%
#'       pool_preds(
#'         n_obs = n_obs,
#'         n_imputes = set_names(rep(n_impute,2), c('old','new')),
#'         miss_strats = set_names(rep(miss_strat,2), c('old','new'))
#'       ) %>%
#'       as.numeric()
#'
#'     output$train_label <- orig_label
#'
#'
#'   }
#'
#'   output$n_impute <- n_impute
#'   output$miss_strat <- miss_strat
#'
#'   class(output) <- "mgb_booster"
#'
#'   return(output)
#'
#' }
#'
#' mgb_train_mi <- function(data_list, nround_list, args){
#'
#'   map2(
#'     .x = data_list,
#'     .y = nround_list,
#'     .f = function(data, nrounds){
#'       args$data <- data
#'       args$nrounds <- nrounds
#'       start <- Sys.time()
#'       output <- do.call(xgboost, args)
#'       stop <- Sys.time()
#'       output %<>% mgb_cmp_time(start = start, stop = stop, args = args)
#'       output
#'     }
#'   )
#'
#' }
#'
#' #' ipa gradient boosting predictions
#' #'
#' #' @description Compute predictions from ipa gradient boosting
#' #'   decision tree ensembles.
#' #'
#' #' @param object an object of class `xgb.Booster`.
#' #' @param newdata an object that inherits one of the following
#' #'   classes: `si_dmat`, `mi_dmat`, or `ipa_dmat`.
#' #' @param outputmargin `TRUE` / `FALSE`. If `TRUE`, the predictions are
#' #'   returned as an untransformed sum of predictions from the boosting
#' #'   ensemble. For example, setting `outputmargin`=`TRUE` for logistic
#' #'   regression would result in predictions for log-odds instead of
#' #'   probabilities.
#' #' @param ntreelimit The number of trees or boosting iterations
#' #'   to be used when forming a sum of predicted values (see Details).
#' #'   If unspecified, all trees in the ensemble will be used.
#' #' @param predleaf `TRUE` / `FALSE`. When `predleaf` = `TRUE`, the output
#' #'   is a matrix object with the number of columns corresponding
#' #'   to the number of trees.
#' #' @param predcontrib `TRUE` / `FALSE`. Whether to return
#' #'   contributions to individual predictions (see Details).
#' #' @param approxcontrib `TRUE` / `FALSE`. Whether to use a fast
#' #'   approximation for feature contributions (see Details).
#' #' @param predinteraction `TRUE` / `FALSE`. Whether to return
#' #'   contributions of feature interactions to individual
#' #'   predictions (see Details).
#' #' @param reshape `TRUE` / `FALSE`. Whether to reshape the
#' #'   vector of predictions to a matrix form when there are
#' #'   several prediction outputs per case. This option has no
#' #'   effect when either of `predleaf`, `predcontrib`, or
#' #'   `predinteraction` flags is TRUE.
#' #'
#' #' @details
#' #' Note that `ntreelimit` is not necessarily equal to the
#' #' number of boosting iterations and it is not necessarily equal
#' #' to the number of trees in a model. E.g., in a random forest-like
#' #' model, `ntreelimit` would limit the number of trees.
#' #' But for multiclass classification, while there are multiple trees
#' #' per iteration, `ntreelimit` limits the number of boosting iterations.
#' #'
#' #' Setting `predcontrib = TRUE` allows to calculate contributions
#' #' of each feature to individual predictions. For "gblinear" booster,
#' #' feature contributions are simply linear terms (feature_beta *
#' #' feature_value). For "gbtree" booster, feature contributions are SHAP
#' #' values (Lundberg 2017) that sum to the difference between the expected
#' #' output of the model and the current prediction (where the hessian
#' #' weights are used to compute the expectations). Setting
#' #' `approxcontrib = TRUE` approximates these values following the idea
#' #' explained in \url{http://blog.datadive.net/interpreting-random-forests/}.
#' #'
#' #' With `predinteraction = TRUE`, SHAP values of contributions of
#' #' interaction of each pair of features are computed. Note that this
#' #' operation might be rather expensive in terms of compute and memory.
#' #' Since it quadratically depends on the number of features, it is
#' #' recommended to perform selection of the most important features first.
#' #' See below about the format of the returned results.
#' #'
#' #' @return
#' #' For regression or binary classification, it returns a vector of
#' #' length `nrow(newdata)`. For multiclass classification, either a
#' #' `num_class * nrow(newdata)` vector or a `(nrow(newdata), num_class)`
#' #'  dimension matrix is returned, depending on the `reshape` value.
#' #'
#' #' When `predleaf = TRUE`, the output is a matrix object with the
#' #' number of columns corresponding to the number of trees.
#' #'
#' #' When `predcontrib = TRUE` and it is not a multiclass setting,
#' #' the output is a matrix object with `num_features + 1` columns.
#' #' The last "+ 1" column in a matrix corresponds to bias.
#' #' For a multiclass case, a list of `num_class` elements is returned,
#' #' where each element is such a matrix. The contribution values are on
#' #' the scale of untransformed margin (e.g., for binary classification
#' #' would mean that the contributions are log-odds deviations from bias).
#' #'
#' #' When `predinteraction = TRUE` and it is not a multiclass setting,
#' #' the output is a 3d array with dimensions
#' #' `c(nrow, num_features + 1, num_features + 1)`. The off-diagonal
#' #' (in the last two dimensions) elements represent different features
#' #' interaction contributions. The array is symmetric WRT the last
#' #' two dimensions. The "+ 1" columns corresponds to bias.
#' #' Summing this array along the last dimension should produce
#' #' practically the same result as predict with `predcontrib = TRUE`.
#' #' For a multiclass case, a list of `num_class` elements is returned,
#' #' where each element is such an array.
#' #'
#' #' @seealso
#' #' [mgb_train()],
#' #' [mgb_cv()],
#' #' and
#' #' [mgb_surv_prob()]
#' #'
#' #' @references
#' #'
#' #' Scott M. Lundberg, Su-In Lee, "A Unified Approach to Interpreting
#' #' Model Predictions", NIPS Proceedings 2017,
#' #' \url{https://arxiv.org/abs/1705.07874}
#' #'
#' #' Scott M. Lundberg, Su-In Lee, "Consistent feature attribution for
#' #' tree ensembles", \url{https://arxiv.org/abs/1706.06060}
#' #'
#' #' @note This function is a wrapper for `predict.xgb.Booster`,
#' #' an un-exported function from the `xgboost` package. The inputs
#' #' and details described here are copied from the documentation in
#' #' `predict.xgb.Booster`.
#'
#' #' @export
#' mgb_predict <- function(
#'   object,
#'   new_data,
#'   new_n_impute,
#'   new_miss_strat,
#'   reshape = FALSE,
#'   predleaf = FALSE,
#'   ntreelimit = NULL,
#'   predcontrib = FALSE,
#'   outputmargin = FALSE,
#'   approxcontrib = FALSE,
#'   predinteraction = FALSE
#' ){
#'
#'   .dots <- list(
#'     reshape = reshape,
#'     predleaf = predleaf,
#'     ntreelimit = ntreelimit,
#'     predcontrib = predcontrib,
#'     outputmargin = outputmargin,
#'     approxcontrib = approxcontrib,
#'     predinteraction = predinteraction
#'   )
#'
#'   miss_strats <- c(old = object$miss_strat, new = new_miss_strat)
#'   n_imputes <- c(old = object$n_impute, new = new_n_impute)
#'
#'   if( miss_strats["old"] == 'mi' ){
#'
#'     if(.dots$predcontrib)
#'       stop("predcontrib must = FALSE for mi data")
#'     if(.dots$predinteraction)
#'       stop("predinteraction must = FALSE for mi data")
#'     if(.dots$predcontrib)
#'       stop("reshape must = FALSE for mi data")
#'
#'   }
#'
#'   switch(
#'     paste(miss_strats, collapse = '_'),
#'     "mi_mi" = mgb_prd_mi_mi(object, new_data, args = .dots),
#'     "mi_si" = mgb_prd_mi_si(object, new_data, args = .dots),
#'     "si_mi" = mgb_prd_si_mi(object, new_data, args = .dots),
#'     "si_si" = mgb_prd_si_si(object, new_data, args = .dots),
#'     "stacked_stacked" = mgb_prd_si_si(object, new_data, args = .dots),
#'     "stacked_si" = mgb_prd_si_si(object, new_data, args = .dots),
#'     stop(
#'       glue(
#'         "Cannot create predictions when training data \\
#'       uses {miss_strats$old} strategy and testing data uses \\
#'       {miss_strats$new} strategy"
#'       )
#'     )
#'   ) %>%
#'     pool_preds(
#'       n_obs = nrow(new_data) / n_imputes["new"],
#'       n_imputes = n_imputes,
#'       miss_strats = miss_strats
#'     )
#'
#' }
#'
#'
#' pool_preds <- function(
#'   preds,
#'   n_obs,
#'   n_imputes,
#'   miss_strats
#' ) {
#'
#'   switch(
#'     paste(miss_strats, collapse = '_'),
#'     'mi_mi' = reduce(preds, `+`) / n_imputes['new'],
#'     'mi_si' = reduce(preds, `+`) / n_imputes['old'],
#'     'si_mi' = reduce(preds, `+`) / n_imputes['new'],
#'     'si_si' = preds,
#'     'stacked_si' = preds,
#'     'stacked_stacked' = tapply(
#'       X = preds,
#'       INDEX = rep(1:n_obs, each = n_imputes['new']),
#'       FUN =  mean)
#'   )
#'
#' }
#'
#'
#' mgb_prd_si_si <- function(object, new_data, args){
#'
#'   args$object  <- object$fit
#'   args$newdata <- new_data
#'   output <- do.call(predict, args)
#'   output
#'
#' }
#'
#' mgb_prd_si_mi <- function(object, new_data, args){
#'
#'   data_list <- prep_data_list(new_data, n_impute = object$n_impute)
#'
#'   args$object  <- object$fit
#'
#'   map(
#'     .x = data_list,
#'     .f = ~ {
#'       args$newdata <- .x
#'       output <- do.call(predict, args)
#'       output
#'
#'     }
#'   )
#'
#'
#' }
#'
#' mgb_prd_mi_mi <- function(object, new_data, args){
#'
#'   data_list <- prep_data_list(new_data, n_impute = object$n_impute)
#'
#'   if(length(data_list) != length(object$fit))
#'     stop(
#'       "number of imputations in training/testing sets do not match",
#'       call. = FALSE
#'     )
#'
#'   map2(
#'     .x = object$fit,
#'     .y = data_list,
#'     .f = ~ {
#'       args$object  <- .x
#'       args$newdata <- .y
#'       output <- do.call(predict, args)
#'       output
#'     }
#'   )
#'
#' }
#'
#' mgb_prd_mi_si <- function(object, new_data, args){
#'
#'   args$newdata <- new_data
#'
#'   map(
#'     .x = object$fit,
#'     .f = ~ {
#'       args$object  <- .x
#'       output <- do.call(predict, args)
#'       output
#'     }
#'   )
#'
#' }
#'
#'
#'
#' #' Baseline hazard function
#' #'
#' #' @description This function is a wrapper for the
#' #'   [gbm::basehaz.gbm()]. The function
#' #'   computes the Breslow estimator of the baseline
#' #'   hazard function for a proportional hazard
#' #'   regression model.
#' #'
#' #' @param label A numeric vector with time-to-event values, where
#' #'   censored observations have negative times and uncensored
#' #'   observations have positive times (see [label_for_survival()]).
#' #' @param predictions The predicted values of the regression model
#' #'   on the log hazard scale.
#' #' @param eval_times Values at which the baseline hazard will
#' #'   be evaluated.
#' #' @param smooth If `TRUE` `mgb_bhaz` will smooth the estimated
#' #'   baseline hazard using Friedman's super smoother
#' #'   [stats::supsmu()].
#' #' @param cumulative If `TRUE` the cumulative survival function
#' #'   will be computed.
#' #' @export
#' #'
#' mgb_bhaz <- function(
#'   mgb_booster,
#'   eval_times = NULL,
#'   smooth = FALSE,
#'   cumulative = TRUE
#' ){
#'
#'   if(any(mgb_booster$train_label == 0))
#'     stop(
#'       "Survival times (i.e. label) must be non-zero",
#'       call. = FALSE
#'     )
#'
#'   basehaz.gbm(
#'     t = get_time(mgb_booster$train_label),
#'     delta = get_status(mgb_booster$train_label),
#'     f.x = mgb_booster$train_predictions,
#'     t.eval = eval_times,
#'     smooth = smooth,
#'     cumulative = cumulative
#'   )
#'
#' }
#'
#' #' Predicted values from `ipa` boosters.
#' #'
#' #' @description Computing survival probabilities with gradient
#' #'   boosted decision tree ensembles.
#' #'
#' #' @inheritParams mgb_predict
#' #' @param eval_times (numeric) a vector of times for which model
#' #'   predictions are computed.
#' #'
#' #' @export
#'
#' mgb_surv_prob <- function(
#'   object,
#'   new_data,
#'   eval_times,
#'   new_n_impute,
#'   new_miss_strat
#' ) {
#'
#'   base_haz <- mgb_bhaz(
#'     mgb_booster = object,
#'     eval_times = eval_times
#'   )
#'
#'   predictions <- mgb_predict(
#'     object = object,
#'     new_data = new_data,
#'     new_n_impute = new_n_impute,
#'     new_miss_strat = new_miss_strat,
#'     outputmargin = TRUE
#'   )
#'
#'   prb <- matrix(
#'     data = 0,
#'     nrow=length(predictions),
#'     ncol=length(eval_times)
#'   )
#'
#'   for(i in 1:length(base_haz)){
#'     prb[,i] <- exp(-exp(predictions) * (base_haz[i]))
#'   }
#'
#'   prb
#'
#' }
#'
