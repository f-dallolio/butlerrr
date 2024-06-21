#' Naming Helpers
#'
#' @param x a vector.
#' @param nm a character vector or a string.
#'
#' @return `set_names` and `as_named` return the renamed input vector. `is_named0` and `is_named` return a single `TRUE` or `FALSE`. `are_*` return a logical vector of the same length as the input. `which_*` return a vector of integers as postions (could be length 0).
#' @name set-names
NULL

#'
#' @rdname set-names
#' @export
set_names <- function(x, nm = x){
  if(length(nm) == 1) nm <- rep.int(nm, length(x))
  setNames(x, as.character(nm))
}
#'
#' @rdname set-names
#' @export
new_names <- function(x, pos, nm){
  x <- as_named(x)
  if(is.character(pos)){
    pos <- which(pos == names(x))
  }
  stopifnot(all(is_whole(pos)))
  stopifnot(max(pos) <= length(x))
  stopifnot(length(pos) <= length(x))
  stopifnot(length(nm) %in% c(1, length(pos)))
  names(x)[pos] <- nm
  x
}
#'
#' @rdname set-names
#' @export
as_named <- function(x){
  nms <- names(x)

  if(is.null(nms)) {
    names(x) <- rep("", length(x))
    return(x)
  }

  na_nms <- which(is.na(nms))
  if(length(na_nms)){
    names(x)[na_nms] <- rep("", length(na_nms))
  }
  x
}

#' Does a vector have names?
#'
#' @param x vector.
#' @param .empty_as_null `TRUE` or `FALSE` (default). If `TRUE`, a vector with empty names is considered unnamed.
#'
#' @return `TRUE` or `FALSE`.
#' @name is-named
NULL
#'
#' @rdname is-named
#' @export
is_named0 <- function(x){
  !is.null(names(x))
}
#'
#' @rdname is-named
#' @export
is_unnamed0 <- function(x){
  !is_named0(x)
}
#'
#' @rdname is-named
#' @export
is_named <- function(x, .empty_as_null = FALSE){
  nms <- names(x)

  if(.empty_as_null && all(names(x) == "")){
    names(x) <- NULL
  }

  is_named0(x)
}
#'
#' @rdname is-named
#' @export
is_unnamed <- function(x, .empty_as_null = FALSE){
  !is_named(x, .empty_as_null = .empty_as_null)
}

#' Are elements of a vector named?
#'
#' @param x vector.
#'
#' @return a logical vector for `are_*` functions. An integer vector for `which_` functions.
#' @name are-named
NULL
#'
#' @rdname are-named
#' @export
are_named <- function(x){
  if(!is_named0(x)){
    return(vector("logical", length(x)))
  }
  nms <- names(x)
  nms != "" & !is.na(nms)
}
#'
#' @rdname are-named
#' @export
are_unnamed <- function(x){
  !are_named(x)
}
#'
#' @rdname are-named
#' @export
which_named <- function(x){
  which(are_named(x))
}
#'
#' @rdname are-named
#' @export
which_unnamed <- function(x){
  which(are_unnamed(x))
}
