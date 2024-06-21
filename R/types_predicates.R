#' Is a number whole?
#'
#' @param x a numeric vector.
#' @param tol tolerance.
#'
#' @return TRUE or FALSE
#' @export
is_whole <-function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

#' Does a character vector only contains numbers?
#'
#' @param x a character vector or an object coercible to one.
#' @param na.omit TRUE or FALSE. If TRUE (default), NAs are omitted.
#'
#' @return TRUE or FALSE.
#' @export
is_numeric_chr <- function(x, na.omit = TRUE) {
  if (na.omit) x <- na.omit(x)
  if (!is.character(x)) x <- as.character(x)
  all(grepl("^[[:digit:]]+$", x))
}

#' Does a character vector only contains integers?
#'
#' @param x a character vector or an object coercible to one.
#' @param na.omit TRUE or FALSE. If TRUE (default), NAs are omitted.
#'
#' @return TRUE or FALSE.
#' @export
is_integer_chr <- function(x, na.omit = TRUE) {
  if (is_numeric_chr(x, na.omit = na.omit)) {
    if (na.omit) x <- na.omit(x)
    rlang::is_integerish(as.numeric(x))
  } else {
    FALSE
  }
}

#' Is object a single string
#'
#' @param x an R object.
#'
#' @return TRUE if `x` is a character of length 1 (i.e. a string) or FALSE.
#' @export
is_string <- function(x){
  is.character(x) && length(x) == 1
}




#' Is an object a base type?
#'
#' @name base-type
NULL
#' @rdname base-type
#' @export
is_base_type <- function(x, class = NULL, type = NULL, n = NULL){
  if(is.null(n)) {
    n_out <- TRUE
  } else {
    n_out <- length(x) == n
  }
  if(is.null(type)){
    out_type <- typeof(x) %in% c("logical", "integer", "double", "complex",
                                 "character", "raw", "list")
  } else {
    out_type <-  typeof(x) == type
  }

  if(is.null(class)) {
    out_class <- class(x) %in%  c("logical", "integer", "double", "complex",
                                  "character", "raw", "numeric")
  } else {
    out_class <-  class(x) == class
  }
  all(n_out, out_type, out_class)
}
#' @rdname base-type
#' @export
is_base_scalar <- function(x, class = NULL, type = NULL){
  is_base_type(x, class = class, type = type, n = 1)
}



fns_list <- function(..., .predicate = NULL, .strict = FALSE){
  x <- list(...)
  are_fns <- function(x) inherits(x, "formula") || is.function(x) || is_string(x)
  no_fn_id <- vapply(x, Negate(are_fns), logical(1))
  if(any(no_fn_id)){
    pos <- which(no_fn_id)
    cls <- vapply(x[pos], class, character(1))
    cls_split <- split(pos, cls)
    cls_vec <- str_as_vector(cls_split)
    cls_nms <- paste0(names(cls_vec), ":")
    cls_plural <- rep("", length(cls_split))
    cls_plural[lengths(cls_split) > 1] <- "s"
    msg1 <- sprintf("All elements must be either a function, a formula, or a string, Not")
    msg2 <- sprintf("- %s  element%s %s", format(cls_nms), cls_plural, cls_vec)
    msg <- paste(c(msg1, msg2), collapse = "\n")
    if(.strict){
      stop(msg)
    } else {
      warning(msg)
    }
  }
  out <- lapply(x, as_function)
  if(is.null(.predicate)){
    .predicate = match.arg(.predicate, c("and", "or"))
  }
  structure(out,.p = .predicate, class = c("fns_list"))
}

and_p <- function(..., .strict = FALSE){
  args <- c(list(...), list(.predicate = "and", .strict = .strict))
  do.call(fns_list, args)
}
or_p <- function(..., .strict = FALSE){
  args <- c(list(...), list(.predicate = "or", .strict = .strict))
  do.call(fns_list, args)
}

obj_is <- function(x, .p, ...){
  if(inherits(.p, "fns_list")){
    out <- vapply(.p, \(f) f(x), logical(1))
    and_or <- attr(.p, ".p")
    if((and_or) == "and") return(all(out))
    return(any(out))
  }
  if(inherits(x, "formula") || is.function(x) || is_string(x)){
    out <- .p(x, ...)
    if(is.logical(out)){
      stopifnot(".p must return a single TRUE or FALSE." = length(out) == 1)
      return(out)
    } else {
      stop(".p must be a function, a formula, a string,
           or a list of functions of class `fns_list`")
    }
  }
}

objs_are <- function(..., .p, .args = NULL){
  x <- list(...)
  sapply(list(...), \(x) do.call(obj_is, append(list(x = x, .p = .p), .args)))
}
objs_every <- function(..., .p, .args = NULL){
  out <- objs_are(..., .p = .p, .args = .args)
  all(out)
}
objs_some <- function(..., .p, .args = NULL){
  out <- objs_are(..., .p = .p, .args = .args)
  any(out)
}
objs_none <- function(..., .p, .args = NULL){
  !objs_some(..., .p = .p, .args = .args)
}
