#' Expressions to Strings or Character Vectors
#'
#' @param x a symbol, or a call.
#' @param .names_type one of `c("full", "sym", "head")`.
#' \describe{
#'    \item{"full"}{The output is equivalent to `deparse(x)`.}
#'    \item{"sym"}{The function returns the string equivalent of a symbol `x` is a symbol or a call with no arguments.}
#'    \item{"head"}{The function returns only the string equivalent of a symbol or the call name.}
#' }
#'
#' @return a string for `symbolic_to_str` or a character vector for
#'   `symbolic_to_chr`.
#' @examples
#' symbolics <- list(quote(mean), quote(mean()), quote(mean(1:10)))
#'
#' symbolic_to_str(symbolics[[1]])
#' symbolic_to_str(symbolics[[2]])
#' symbolic_to_str(symbolics[[3]])
#'
#' # Different `.names_type`
#' symbolic_to_str(symbolics[[2]], .names_type = "full")
#'
#' symbolic_to_str(symbolics[[2]], .names_type = "sym")
#' symbolic_to_str(symbolics[[3]], .names_type = "sym")
#'
#' symbolic_to_str(symbolics[[3]], .names_type = "head")
#'
#' # `symbolic_to_chr` converts a list of symbolics in a vector of characters.
#' symbolic_to_chr(symbolics)
#' @name symbolics-to-strings
NULL
#'
#' @rdname symbolics-to-strings
#' @export
lang_to_str <- function(x, .names_type = c("full", "sym", "head")) {
  stopifnot("Input must be a symbol or a call." = rlang::is_symbolic(x))

  .names_type <- match.arg(.names_type)

  n <- length(as.list(x))

  if (.names_type == "full" || is_symbol2(x)) {
    return(deparse(x))
  }

  if (.names_type == "head" || n == 1) {
    x <- x[[1]]
  }
  deparse(x)
}
symbolic_to_str <- lang_to_str
#'
#' @rdname symbolics-to-strings
#' @export
lang_to_chr <- function(x, .names_type = c("full", "sym", "head")) {
  if (is.vector(x)) {
    vapply(x, symbolic_to_str, character(1), .names_type = .names_type)
  } else {
    symbolic_to_str(x, .names_type = .names_type)
  }
}
symbolic_to_chr <- lang_to_chr
