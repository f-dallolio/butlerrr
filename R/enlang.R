#' Convert to `"symbolic"` objects (symbols and calls)
#'
#' @param x .
#' @param ... .
#' @param .x .
#' @param .out_type .
#' @param .strict .
#' @param .simplify .
#' @param .no_ns .
#' @param .no_args .
#'
#' @return [enlang()] returns a symbolic object. [enlangs()] returns a list of
#'   symbolic objects.
#' @name enlang
NULL
#' @rdname enlang
#' @export
enlang <- function(x,
                   .out_type = NULL, .strict = FALSE,
                   .simplify = FALSE, .no_ns = FALSE, .no_args = FALSE){
  if(is_string(x)){
    out <- str_as_lang(x, .out_type =.out_type, .strict = .strict)
    return(out)
  }
  if(is_base_type(x, n = 1)) {
    out <- str_as_lang(deparse(x), .out_type =.out_type, .strict = .strict)
    return(out)
  }
  if(typeof(x) %in% c("closure", "special", "builtin")){
    out <- fn_as_lang(x, .out_type = .out_type, .strict = .strict,
                      .simplify = .simplify, .no_ns = .no_ns, .no_args = .no_args)
    return(out)
  }
  if(is_symbolic(x)){
    if(is.null(.out_type)){
      return(x)
    } else {
      out <- as.list(x)
    }
    .out_type |> match.arg(.out_type)
    choices <- c("sym", "syms", "symbol", "symbols", "name",
                 "call", "calls", "language", "lang")
    out_type <- match.arg(.out_type, choices)
    if(out_type %in% c("sym", "syms", "symbol", "symbols", "name")){
      return(out[[1]])
    } else {
      return(as.call(out))
    }
  }
  cls <- class(x)
  msg <- sprintf(
    "Cannot convert an objec of class \"%s\" into a \"symbolic\" object.", cls
  )
  stop(msg)
}
#' @rdname enlang
#' @export
enlangs <- function(..., .x =NULL,
                    .out_type = NULL, .strict = FALSE,
                    .simplify = FALSE, .no_ns = FALSE, .no_args = FALSE){
  x <- append(list(...), .x)
  out <- lapply(X = x,
                FUN = lang,
                .out_type = .out_type, .strict = .strict,
                .simplify = .simplify, .no_ns = .no_ns, .no_args = .no_args)
  out
}
