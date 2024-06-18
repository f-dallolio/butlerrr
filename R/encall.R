#' Convert to Named Calls (e.g. `func()`)
#'
#' @inheritParams enlang
#'
#' @return TODO.
#' @name encall
NULL
#' @name encall
#' @export
encall <- function(x, .strict = FALSE,.simplify = FALSE, .no_ns = FALSE, .no_args = FALSE){
  out <- enlang(x,
         .strict = .strict,
         .simplify = .simplify,
         .no_ns = .no_ns,
         .no_args = .no_args)
  if(is_call2(out)) return(out)
  as.call(list(out))
}

#' @name encall
#' @export
encalls <- function(..., .x =NULL, .named = NULL, .name_types = NULL, .strict = FALSE, .simplify = FALSE, .no_ns = FALSE, .no_args = FALSE){
  x <- append(list(...), .x)
  out <- lapply(X = x,
                FUN = encall,
                .strict = .strict,
                .simplify = .simplify,
                .no_ns = .no_ns,
                .no_args = .no_args)
  if(is.null(.named)) return(out)
  lang_auto_name(out, .names_type = .name_types)
}
