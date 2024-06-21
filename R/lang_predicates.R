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
#' @name lang-is-namespaced
NULL
#'
#' @rdname lang-is-namespaced
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
#' @rdname lang-is-namespaced
#' @export
are_ns_sym <-  function(x, ns = NULL, private = NULL) {
  if(!is.vector(x)) x <- list(x)
  if(list_only) {
    stopifnot(is.list(x))
  } else {
    stopifnot(is.recursive(x))
  }
  vapply(x, is_ns_sym, logical(1), ns = ns, private = private)
}
#'
#' @rdname lang-is-namespaced
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
#'
#' @rdname lang-is-namespaced
#' @export
are_ns_call <- function(x, ns = NULL, private = NULL) {
  if(!is.vector(x)) x <- list(x)
  if(list_only) {
    stopifnot(is.list(x))
  } else {
    stopifnot(is.recursive(x))
  }
  vapply(x, is_ns_call, logical(1), ns = ns, private = private)
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
#' @name lang-is-sym-call
NULL
#'
#' @rdname lang-is-sym-call
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
#' @rdname lang-is-sym-call
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
#'
#' @rdname lang-is-sym-call
#' @export
is_call_simple <- function(x, ns = NULL){

  if (inherits(x, "formula")) {
    x <- as.list(x)[-1]
  }

  if(!is.call(x) || is_ns_sym(x)){
    return(FALSE)
  }

  head <- x[[1]]
  ns_head <- is_ns_sym(head)

  if(!is.null(ns) && !identical(ns_head, ns)){
    return(FALSE)
  }

  is.symbol(head) || ns_head
}

#' Is an object a call or a symbol?
#'
#' @param x an arbitrary R object.
#'
#' @return TRUE or FALSE or a vector of logicals.
#' @name lang-is-symbolic
NULL
#'
#' @rdname lang-is-symbolic
#' @export
is_symbolic <- function(x) {
  typeof(x) %in% c("language", "symbol")
}
#'
#' @rdname lang-is-symbolic
#' @export
are_symbolic <- function(x, list_only = FALSE) {
  if(list_only) {
    stopifnot(is.list(x))
  } else {
    stopifnot(is.recursive(x))
  }
  vapply(x, is_symbolic, logical(1))
}

#' Is an object a syntactic literal?
#'
#' @param x an arbitrary R object.
#'
#' @return TRUE or FALSE or a vector of logicals.
#' @name lang-is-syntactic-literal
NULL
#'
#' @rdname lang-is-syntactic-literal
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
#'
#' @rdname lang-is-syntactic-literal
#' @export
are_syntactic_literal <- function(x, list_only = FALSE) {
  if(list_only) {
    stopifnot(is.list(x))
  } else {
    stopifnot(is.recursive(x))
  }
  vapply(x, is_syntactic_literal, logical(1))
}
#' Is an object an expression?
#'
#' @param x an arbitrary R object.
#'
#' @return TRUE or FALSE or a vector of logicals.
#' @name lang-is-expr
NULL
#'
#' @rdname lang-is-expr
#' @export
is_expr <- function(x){
  is_symbolic(x) || is_syntactic_literal(x)
}
#'
#' @rdname lang-is-expr
#' @export
are_expr <- function(x, list_only = FALSE) {
  if(list_only) {
    stopifnot(is.list(x))
  } else {
    stopifnot(is.recursive(x))
  }
  vapply(x, is_expr, logical(1))
}

#' Is an object callable?
#'
#' @param x an arbitrary R object.
#'
#' @return TRUE or FALSE or a vector of logicals.
#' @name lang-is-callable
NULL
#'
#' @rdname lang-is-callable
#' @export
is_callable <- function(x){
  is_symbolic(x) || is.function(x)
}
#'
#' @rdname lang-is-callable
#' @export
are_callable <- function(x, list_only = FALSE) {
  if(list_only) {
    stopifnot(is.list(x))
  } else {
    stopifnot(is.recursive(x))
  }
  vapply(x, is_callable, logical(1))
}
