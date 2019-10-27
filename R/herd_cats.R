
#' Herding Cats!
#'
#' @description `xgboost` (and many other) modeling functions
#'   expect matrix input with factor levels one-hot encoded.
#'
#'   `cat_spread` will one-hot encode any factor or character variable
#'   in `data` and return a one-hot encoded `tibble`. Alternatively,
#'   `cat_gather` will apply the inverse operation and convert one-hot
#'   encoded columns back into factors.
#'
#' @param data data with categorical variables (i.e., factors)
#'   that need to be spread or gathered.
#'
#' @param factor_levels This parameter is only relevant for
#'   `cat_gather`. A named list of factor levels, with each
#'    name corresponding to the column in the data that the
#'    factor levels describe.
#'
#' @inheritDotParams mltools::one_hot sparsifyNAs naCols dropCols
#'   dropUnusedLevels
#'
#' @return a [tibble][tibble::tibble-package] with categorical variables
#'   herded as you like.
#'
#' @examples
#'
#' df <- data.frame(x = rep(letters[1:2], 50), y = 1:100)
#'
#' one_hot_df <- cat_spread(df)
#' cat_gather(one_hot_df, factor_levels = list(x=c('a','b')))
#'
#'
#' @export

cat_spread <- function(data, ...){


  data %<>% dplyr::mutate_if(is.character, as.factor)

  if(!any(purrr::map_lgl(data, is.factor))) return(data)

  factor_variables <- get_factors(data)

  nlevels <- purrr::map_int(
    .x = purrr::set_names(factor_variables),
    .f = ~ length(levels(data[[.x]]))
  )

  fctrs_to_list <- nlevels == vctrs::vec_size(data)

  if(any(fctrs_to_list)){

    fctrs_to_list <- names(fctrs_to_list)[which(fctrs_to_list)]

    warning(
      'Some factors have number of levels = rows in data: ',
      list_things(fctrs_to_list),
      ' \nDid you remember to remove the ID columns?'
    )
  }

  .dots <- list(...) %>%
    check_dots(
      valid_args =  c(
        'sparsifyNAs',
        'naCols',
        'dropCols',
        'dropUnusedLevels'
      )
    )

  .dots$dt <- data.table::as.data.table(data)
  .dots$cols <- factor_variables

  output <- do.call(mltools::one_hot, args = .dots)

  tibble::as_tibble(output)

}

#' @rdname cat_spread
#' @export
cat_gather <- function(
  data,
  factor_levels
) {

  old_names <- new_names <- names(data)

  factor_names <- factor_levels %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = value) %>%
    dplyr::mutate(factor_name = paste(name, value, sep = "_")) %>%
    dplyr::select(factor_name, name) %>%
    tibble::deframe()

  new_names <- unique(dplyr::recode(old_names, !!!factor_names))

  for(i in seq_along(factor_levels)){

    .factor <- names(factor_levels)[i]
    .levels <- factor_levels[[i]]
    .names <- paste(.factor, .levels, sep = '_')

    new_col <- data[, .names] %>%
      apply(1, which.max) %>%
      as.numeric() %>%
      factor(levels = 1:length(.names), labels = .levels)

    data[[.factor]] <- new_col
    data[, .names] <- NULL

  }

  data[, new_names]

}

#' transfer factor levels
#'
#' @description take the factor levels in training data
#'   and copy them over to testing data. This is an important
#'   pre-processing step for data splits that may have
#'   different factor levels in training and testing sets.
#'
#' @param to the data that factor levels are transferred to
#' @param from the data that factor levels are transferred from
#'
#' @note `to` and `from` must have the same factor columns. For example,
#'   if `to` has a factor named `A` and `from` does not have a factor
#'   of the same name, the function will stop and tell you which
#'   factor variables are missing.
#'
#' @export
#'
cat_transfer <- function(to, from){

  # check that the two frames have the same factor variables

  fctrs_to <- get_factors(to)
  fctrs_from <- get_factors(from)

  fctrs_only_in_to <- setdiff(fctrs_to, fctrs_from)
  fctrs_only_in_from <- setdiff(fctrs_from, fctrs_to)

  if(!rlang::is_empty(fctrs_only_in_to)){
    stop(
      paste(
        "to has some factors that are not in from:",
        list_things(fctrs_only_in_to)
      )
    )
  }

  if(!rlang::is_empty(fctrs_only_in_from)){
    stop(
      paste(
        "from has some factors are not in to:",
        list_things(fctrs_only_in_from)
      )
    )
  }

  levels_from <- purrr::map(
    .x = purrr::set_names(fctrs_from, fctrs_from),
    .f = ~ levels(from[[.x]])
  )

  for(f in names(levels_from)){
    to[[f]] %<>% factor(levels = levels_from[[f]])
  }

  return(to)

}

get_factors <- function(data){
  purrr::map_lgl(data, is.factor) %>%
    which() %>%
    names()
}
