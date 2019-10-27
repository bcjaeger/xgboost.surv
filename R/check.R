
list_things <- function(things){

  glue::glue_collapse(things, sep = ', ', last = ' and ')

}

check_dots <- function(.dots, valid_args){

  bad_args <- setdiff(names(.dots), valid_args)

  if(!rlang::is_empty(bad_args)){
    stop(
      paste(
        "The following arguments are unrecognized:",
        list_things(bad_args)
      ),
      call. = FALSE
    )
  }

  .dots

}
