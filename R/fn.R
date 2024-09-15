#' Function Namespace
#'
#' @param x A function, list of functions, or a string/character vector with function name(s).
#' @param ... empty dots.
#'
#' @return The namespace or a list of namespaces of the function in input.
#' @name fn-ns
#'
#' @examples
#' fn_ns(mean)
#' fn_ns("mean")
#' fn_ns(c("mean", var))
NULL

#' @rdname fn-ns
#' @export
fn_ns <- function(x, ...) {
  if(is.vector(x) && length(x) > 1){
    out <- lapply(x, fn_ns)
    return(out)
  }
  stopifnot(is.character(x) || is.function(x))
  if(is.character(x) && length(x) == 1){
    x <- fn_from_chr(x)
  }
  env <- environment(x) %||% asNamespace("base")
  out <- if(isNamespace(env)) env else NULL
  return(out)
}

#' @rdname fn-ns
#' @export
fn_ns_name <- function(x, ...){
  if(is.vector(x) && length(x) > 1){
    out <- unlist(lapply(x, fn_ns_name))
    return(out)
  }
  ns <- fn_ns(x)
  if(is.null(ns)) return(NULL)
  environmentName(ns)
}

#' Function from Name
#'
#' @param x A string or a character vector. Also accepts input of the type "ns:::fn".
#' @param ... Empty dots.
#' @param stop_not_found If `FALSE` (default) it returns `NULL` if the function is not found. An error if `TRUE`.
#'
#' @return A function or a list of functions.
#' @export
#'
#' @examples
#' # fn_from_chr("mean")
#' # fn_from_chr(c("mean", "person"))
#' # fn_from_chr(c("utils::person", "utils:::person"))

fn_from_chr <- function(x, ..., stop_not_found = FALSE){
  stopifnot(is.character(x))
  if(length(x) > 1) {
    out <- lapply(x, fn_from_chr, stop_not_found = stop_not_found)
    names(out) <- x
    return(out)
  }

  if(grepl(":", x)){
    fn_call <- tryCatch(str2lang(x), error = function(e) as.symbol(x))
    out <- eval(fn_call, envir = topenv())
    return(out)
  }

  out <- get0(x, envir = topenv(), mode = "function", ifnotfound = NULL)

  if(length(out)) return(out)
  if(stop_not_found) {
    stop(sprintf("Cannot find a funcation called \"%s\"", x))
  } else {
    NULL
  }
}


#' Get Function Name from Function
#'
#' @param x A function or list of functions (also as string or character vector).
#' @param ... Empty dots.
#'
#' @return A string or a character vector.
#' @name fn-name
#'
#' @examples
#' # fn_name("mean")
#' # fn_name2("mean")
#'
#'@rdname fn-name
#'@export
fn_name <- function(x, ...) {

  if(is.list(x)) {
    out <- vapply(x, fn_name, "")
    return(out)
  }

  if(is.character(x)) {
    x <- fn_from_chr(x, stop_not_found = TRUE)
  }

  fns <- ns_get_fns(x, exports_only = TRUE)
  nms <- names(fns)
  out <- nms[vapply(fns, identical, logical(1), x)]

  if(length(out) == 0) {
    fns <- ns_get_fns(x, exports_only = FALSE)
    nms <- names(fns)
    out <- nms[vapply(fns, identical, logical(1), x)]
  }

  if(length(out) == 0) {
    stop("Cannot find funciton name")
  } else if (length(out) > 1) {
    return(out[[1]])
  }

  out

}

#'@rdname fn-name
#'@export
fn_name2 <- function(x, ..., with_ns = TRUE) {
  if(with_ns) return(fn_name_add_ns(x))
  fn_name(x)
}

fn_is_ns_export <- function(x, ...){
  if(is.vector(x) && length(x) > 1){
    out <- vapply(x, fn_is_ns_export, logical(1))
    return(out)
  }
  ns_nm <- fn_ns_name(x)
  fn_nm <- fn_name(x)
  fn_nm %in% getNamespaceExports(ns_nm)
}

fn_name_add_ns <- function(x, ..., export = "::", private = ":::"){

  if(is.vector(x) && length(x) > 1){
    out <- vapply(x, fn_name_add_ns, character(1))
    return(out)
  }

  fn_nm <- fn_name(x)
  ns <- fn_ns(x)
  ns_nm <- fn_ns_name(x)

  if(!isNamespace(ns = ns)) return(fn_nm)
  sep <- if(fn_is_ns_export(x)) export else private
  paste(ns_nm, fn_nm, sep = sep)

}


