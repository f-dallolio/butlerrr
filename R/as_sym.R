#' Convert to Symbol(s)
#'
#' @inheritParams enlang
#'
#' @return TODO.
#' @name as_sym
NULL
#' @name as_sym
#' @export
as_sym <- function(x, .strict = FALSE,.simplify = FALSE, .no_ns = FALSE, .no_args = TRUE){
  out <- encall(x, .strict = .strict, .simplify = .simplify, .no_ns = .no_ns, .no_args = .no_args)
  out <- out[[1]]
  if(is_symbol2(out)) return(out)
  as.symbol(out)
}

#' @name as_sym
#' @export
as_syms <- function(..., .x =NULL, .named =NULL, .name_types = NULL, .strict = FALSE, .simplify = TRUE, .no_ns = FALSE, .no_args = FALSE){
  x <- append(list(...), .x)
  out <- lapply(X = x,
                FUN = as_sym,
                .strict = .strict,
                .simplify = .simplify,
                .no_ns = .no_ns,
                .no_args = .no_args)
  if(is.null(.named)) return(out)
  lang_auto_name(out, .names_type = .name_types)
}

