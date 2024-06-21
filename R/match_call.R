#' Call Matching (similar to [rlang::call_match])
#'
#' @inheritParams base::match.call
#' @param defaults `TRUE` or `FALSE` (default) .If `TRUE`, missing arguments are matched to their defaults.
#' @param ignore_missing `TRUE` or `FALSE` (default). If `TRUE`, missing
#'   arguments without a default value are removed from the matched call.
#' @param expand_dots logical. Should arguments matching ... in the call be
#'   included or left as a ... argument?
#' @param envir an environment, from which the ... in call are retrieved, if
#'   any.
#'
#' @return a call.
#' @name match-call
NULL
#'
#' @rdname match-call
#' @export
match_call <- function(definition = sys.function(sys.parent()),
                       call = sys.call(sys.parent()),
                       defaults = FALSE,
                       ignore_missing = FALSE,
                       expand_dots = TRUE,
                       envir= parent.frame(2L)){

  # if (is.null(definition)) {
  #   stop("`fn` must be supplied.")
  # }
  if (!is.environment(envir)) {
    stop("`envir` must be an environment.")
  }

  if (is.primitive(definition)) {
    return(call)
  }

  call <- match.call(definition, call, expand.dots = FALSE, envir = envir)

  if (defaults) {
    fmls <- formals(definition)
    names <- names(fmls)
    missing <- !names %in% names(call)

    args <- c(as.list(call[-1]), fmls[missing])
    args <- args[names]
    call <- as.call(append(list(call[[1]]), args))
  }

  if (identical(call$..., quote(expr = ))) {
    call$... <- NULL
    return(call)
  }

  if (!expand_dots) {
    return(call)
  }

  i <- match("...", names(call))
  if (is.na(i)) {
    return(call)
  }

  call <- as.list(call)
  out <- c(
    call[seq2(1, i - 1)],
    call$...,
    call[seq2(i + 1, length(call))]
  )

  if(ignore_missing){
    missing_args <- vapply(out, identical, logical(1), quote(expr = ))
    out <- out[!missing_args]
  }

  as.call(out)

}
#'
#' @rdname match-call
#' @export
match_call_self <- function(call,
                            defaults = FALSE,
                            ignore_missing = FALSE,
                            expand_dots = TRUE,
                            envir= parent.frame(2L)){
  fn <- eval(call[[1]], envir = parent.frame())
  match_call(definition = fn,
             call = call,
             defaults = defaults,
             ignore_missing = ignore_missing,
             expand_dots = expand_dots,
             envir = envir)
}


seq2 <- function(from, to) {
  if (length(from) != 1) {
    stop(sprintf("`from` must be length one. %s has length %i.",
                 deparse(substitute(from)),
                 length(from)))
  }
  if (length(to) != 1) {
    stop(sprintf("`to` must be length one. %s has length %i.",
                 deparse(substitute(to)),
                 length(to)))
  }

  if (from > to) {
    integer()
  } else {
    seq.int(from, to)
  }
}
#
# list_flatten_dots <- function(x, .named = FALSE){
#   i <- which(names(x) == "...")
#   pre_dots <- x[seq2(1, i - 1)]
#   post_dots <- x[seq2(i + 1, length(x))]
#   dots <- x[[i]]
#   nms_dots <- names(dots)
#   nms_flag <- nms_dots %in% names(x[-1])
#   nms_dots[nms_flag] <- paste("...", nms_dots[nms_flag], sep = "_")
#
#   if(.named){
#     nms_new <- paste("...",seq_along(x[["..."]]), sep = "_")
#     if(is.null(nms_dots)){
#       nms_dots <- nms_new
#     } else {
#       nms_dots[nms_dots == ""] <-  nms_new[nms_dots == ""]
#     }
#   }
#   names(dots) <- nms_dots
#   append(pre_dots, append(dots, post_dots))
# }
