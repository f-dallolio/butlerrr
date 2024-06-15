#' Automatic Names for Lists of Expressions
#'
#' @inheritParams lang_to_str
#'
#' @param x a list of symbolic objects.
#' @return a named list of symbolics.
#'
#' @export
lang_auto_name <- function(x,
                           .name_all = FALSE,
                           .names_type = c("full", "sym", "head")) {
  if (rlang::is_symbolic(x)) {
    setNames(c(x), symbolic_to_str(x, .names_type = .names_type))
  } else {
    nms <- names(x)
    if (is.null(nms) || .name_all) {
      names(x) <- nms <- rep("", length(x))
    }
    pos <- nms == ""
    names(x)[pos] <- symbolic_to_chr(x[pos], .names_type = .names_type)
    x
  }
}

symbolic_auto_name <- lang_auto_name
