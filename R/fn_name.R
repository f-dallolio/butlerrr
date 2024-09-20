#' Function(s) Name(s)
#'
#' @param x A function, a formula, a string (passed to
#'   or a list of these. Unlike
#'   [`rlang::as_function`][rlang::as_function()], it also accepts a strings indicating function
#'   names including namespace (i.e. with `::` or `:::`).
#'
#' @param env Environment in which to fetch the function.
#'
#' @return A string or a character vector.
#'
#'   [fn_name] returns the simple name of the function(s) if it is exported from
#'   its namespace or the function environment is not a package namespace. If
#'   not, the name of the function is reported in the form
#'   "pkg:::fn".
#'
#'   [fn_name0] always returns only the function name.
#'
#'   [fn_name2] always returns the function name in the form
#'   "pkg::fn" if the function is an exported object and "pkg:::fn" if not.
#'
#' @name fn-name
#' @examples
#' fn1 <- function(x) x
#' fn2 <- base::mean
#' fn3 <- rlang:::call_add_namespace
#'
#' fn_name(fn1)
#' fn_name(fn2)
#' fn_name(fn3)
#'
#' fn_name0(fn1)
#' fn_name0(fn2)
#' fn_name0(fn3)
#'
#' fn_name0(fn1)
#' fn_name0(fn2)
#' fn_name0(fn3)
NULL

#' @rdname fn-name
#' @export
fn_name <- function(x, ..., env = global_env()) {
  stopifnot("Dots must be empty" = ...length() == 0)
  .fn_get_name(x = x, exports_ns = FALSE, private_ns = TRUE, env = env)
}
#' @rdname fn-name
#' @export
fn_name0 <- function(x, ..., env = global_env()) {
  stopifnot("Dots must be empty" = ...length() == 0)
  .fn_get_name(x = x, exports_ns = FALSE, private_ns = FALSE, env = env)
}
#' @rdname fn-name
#' @export
fn_name2 <- function(x, ..., env = global_env()) {
  stopifnot("Dots must be empty" = ...length() == 0)
  .fn_get_name(x = x, exports_ns = TRUE, private_ns = TRUE, env = env)
}

#' @rdname fn-name
#' @export
fn_names <- function(..., env = global_env()) {
  x <- list2(...)
  vapply(x, fn_name, "", env = env)
}
#' @rdname fn-name
#' @export
fn_names0 <- function(..., env = global_env()) {
  x <- list2(...)
  vapply(x, fn_name0, "", env = env)
}
#' @rdname fn-name
#' @export
fn_names2 <- function(..., env = global_env()) {
  x <- list2(...)
  vapply(x, fn_name2, "", env = env)
}

.fn_get_name <- function(x, exports_ns = FALSE, private_ns = TRUE, env = global_env()) {
  if (is_symbolic(x)) {
    return(format(x))
  }
  if (is_quosure(x)) {
    return(format(quo_get_expr(x)))
  }
  if (is_string(x)) {
    x <- tryCatch(str2lang(x), error = as.symbol(x))
    x <- eval(x, envir = env)
  }
  fn <- as_function(x, env = env)
  if (is_lambda(fn)) {
    out <- format(call("~", fn_body(fn)[[2]]))
    names(out) <- ""
    return(out)
  }
  ns <- fn_env(fn) %||% env
  fns <- ns_get_list(.x = ns, .p = is.function)
  out <- names(fns)[vapply(fns, identical, logical(1), fn)]
  if (!is_namespace(ns)) return(out)
  .fn_name_add_ns(out, ns_env_name(ns), exports_ns = exports_ns, private_ns = private_ns)
}

.fn_name_is_export <- function(nms, nss = names(nms)) {
  if (length(nms) == 1 && length(nss) == 1) {
    out <- nms %in% getNamespaceExports(nss)
  } else {
    out <- Vectorize(.fn_name_is_export)(nms, nss)
  }
  unname(out)
}

.fn_name_add_ns <- function(nms, nss, exports_ns = FALSE, private_ns = TRUE) {
  out <- nms
  exp_id <- .fn_name_is_export(nms, nss)
  pvt_id <- !exp_id
  if (exports_ns) out[exp_id] <- paste0(nss[exp_id], "::", out[exp_id])
  if (private_ns) out[pvt_id] <- paste0(nss[pvt_id], ":::", out[pvt_id])
  out
}



fn_body_simple <- function (fn = caller_fn()) {
  stopifnot(is_closure(fn))
  body <- fn_body(fn)
  body2 <- call_args(body)
  if(is_lambda(fn) || length(body2) == 1) {
    body2[[1]]
  } else {
    body
  }
}


