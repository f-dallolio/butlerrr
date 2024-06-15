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

#' Does a language object include namespace?
#'
#' @param x an R object.
#' @param ns a string indicating a namespace name.
#' @param private NULL, TRUE, or FALSE.
#' \describe{
#'    \item{NULL (default)}{The function returns TRUE whether the object is exported from the namespace or not.}
#'    \item{TRUE}{The function returns TRUE if it represents an object not exported from the namespace (i.e. `:::`).}
#'    \item{FALSE}{The function returns TRUE if it represents an object not exported from the namespace (i.e. `::`).}
#' }
#'
#' @return TRUE or FALSE.
#' @name namespaced-language
NULL
#'
#' @rdname namespaced-language
#' @export
is_ns_sym <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") {
    return(FALSE)
  }
  if (!is.null(ns) && !identical(x[[2]], str2lang(ns))) {
    return(FALSE)
  }
  head <- x[[1]]
  if (is.null(private)) {
    identical(head, quote(`::`)) || identical(head, quote(`:::`))
  } else if (private) {
    identical(head, `:::`)
  } else {
    identical(head, `::`)
  }
}
#'
#' @rdname namespaced-language
#' @export
is_ns_call <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") {
    return(FALSE)
  }
  if (!is_ns_sym(x[[1]], ns, private)) {
    return(FALSE)
  }
  TRUE
}



#' Extensions of [rlang::is_symbol()] and [rlang::is_call()]
#'
#' @inheritParams rlang::is_call
#' @param ns_sym_is_sym TRUE or FALSE. If TRUE (default), the function returns
#'   TRUE for symbols and a call of the type pkg::fun. FALSE otherwise.
#' @param ns_sym_is_call TRUE or FALSE. If TRUE (default), the function returns
#'   TRUE for a calls and a call of the type pkg::fun(). FALSE otherwise.
#'
#' @return TRUE or FALSE.
#' @name is-symbol-call-2
NULL
#'
#' @rdname is-symbol-call-2
#' @export
is_symbol2 <- function(x, name = NULL, ns_sym_is_sym = TRUE) {
  if (is_ns_sym(x)) {
    if (ns_sym_is_sym) {
      x <- x[[3]]
    } else {
      return(FALSE)
    }
  }

  if (typeof(x) != "symbol") {
    return(FALSE)
  }

  if (is.null(name)) {
    return(TRUE)
  }

  rlang::as_string(x) %in% name
}
#'
#' @rdname is-symbol-call-2
#' @export
is_call2 <- function(x, name = NULL, n = NULL, ns = NULL, ns_sym_is_call = FALSE) {
  if (is_ns_sym(x)) {
    if (ns_sym_is_call) {
      x <- as.call(list(x))
    } else {
      return(FALSE)
    }
  }
  rlang::is_call(x, name = name, n = n, ns = ns)
}

#' Is an object a call or a symbol?
#'
#' @param x an arbitrary R object.
#'
#' @return TRUE or FALSE or a vector of logicals.
#' @name is-symbolic2
NULL
#'
#' @rdname is-symbolic2
#' @export
is_symbolic <- function(x) {
  typeof(x) %in% c("language", "symbol")
}
#'
#' @rdname is-symbolic2
#' @export
is_symbolic2 <- function(x, all = TRUE) {
  if (all) {
    all2(x, is_symbolic)
  } else {
    vapply(x, is_symbolic, logical(1))
  }
}

#' Is an object a syntactic literal?
#'
#' @param x an arbitrary R object.
#'
#' @return TRUE or FALSE or a vector of logicals.
#' @export
is_syntactic_literal <- function(x) {
  switch(typeof(x),
         NULL = TRUE,
         logical = function(x) length(x) == 1,
         integer = function(x) length(x) == 1,
         double = function(x) length(x) == 1,
         character = function(x) length(x) == 1,
         complex = function(x) {
           if (length(x) != 1) return(FALSE)
           is_na(x) || Re(x) == 0},
         FALSE
  )
}


is_base_type <- function(x, class = NULL, type = NULL, n = NULL){
  if(is.null(n)) {
    n_out <- TRUE
  } else {
    n_out <- length(x) == n
  }
  if(is.null(type)){
    out_type <- typeof(x) %in% c("logical", "integer", "double", "complex",
                                 "character", "raw", "list")
  }
  if(is.null(class)) {
    out_class <- class(x) %in%  c("logical", "integer", "double", "complex",
                                  "character", "raw", "numeric")
  }
  all(n_out, out_type, out_class)
}

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
