

#' @keywords internal
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang %||% !! !!!
#'
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      "name",
      "value",
      "factor_name",
      "time",
      "status",
      "time"
    )
  )


# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

