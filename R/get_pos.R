#' Extract Parts of an Object
#'
#' @param x An R object.
#' @param ... Integers, string or character vector, or unquoted selection expression. Passed to
#'   [`tidyselect::eval_select`][tidyselect::eval_select()].
#'
#'   - If input is an **unnamed atomic vector**, selection is performed on the input itself.
#'
#'   - For a **named atomic vector**, set `unname = TRUE` to perform selection on the input itself.
#'
#' @param simplify_scalar `TRUE` (default) or `FALSE`. If `TRUE` and the output
#'   is a vector of length 1, it returns the output simplified to a scalar.
#' @param unname `TRUE` or `FALSE` (default). If `TRUE` and input is a named atomic vector, selection is always performed on the input itself.
#'
#' @return The extracted elementsÎ of `x`.
#' @name get-pos
#'
NULL

#' @rdname get-pos
#' @export
get_pos0 <- function(x, ..., unname = FALSE) {
  nms <- names(x)
  if(is.atomic(x) && unname) names(x) <- NULL

  names(x) <- nms %||% x
  pos <- tidyselect::eval_select(substitute(c(...)), x)
  if(is.null(nms)) return(unname(x)[pos])
  setNames(x[pos], names(pos))
}
#' @rdname get-pos
#' @export
get_pos <- function(x, ..., simplify_scalar = TRUE, unname = FALSE) {
  out <- get_pos0(x, ..., unname = unname)
  if(simplify_scalar && length(out) == 1) return(out[[1]])
  out
}
#' @rdname get-pos
#' @export
get_pos2 <- function(x, ..., unname = TRUE) {
  stopifnot(...length() == 1)
  out <- get_pos0(x, ..., unname = unname)
  if(length(out) == 1) out[[1]] else out
}
