#' Convert to `"expressions"` (similar to [rlang::expr()])
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
enlang <- function(x, .strict = FALSE, .simplify = FALSE, .no_ns = FALSE, .no_args = FALSE){
  if(is_symbolic(x)) return(x)
  if(is_string(x)){
    out <- str_as_lang(x, .strict = .strict)
    return(out)
  }
  if(is.atomic(x) && length(x) == 1) {
    out <- str_as_lang(as.character(x), .strict = .strict)
    return(out)
  }
  if(typeof(x) %in% c("closure", "special", "builtin")){
    out <- fn_as_lang(x, .strict = .strict,
                      .simplify = .simplify, .no_ns = .no_ns, .no_args = .no_args)
    return(out)
  }

  cls <- class(x)
  msg <- sprintf(
    "Cannot convert an objec of class \"%s\" into a \"symbolic\" object.", cls
  )
  stop(msg)
}
#' @rdname enlang
#' @export
enlangs <- function(..., .x =NULL, .named = NULL, .name_types = NULL, .strict = FALSE, .simplify = FALSE, .no_ns = FALSE, .no_args = FALSE){
  x <- append(list(...), .x)
  out <- lapply(X = x,
                FUN = enlang,
                .strict = .strict,
                .simplify = .simplify,
                .no_ns = .no_ns,
                .no_args = .no_args)
  if(is.null(.named)) return(out)
  lang_auto_name(out, .names_type = .name_types)
}
