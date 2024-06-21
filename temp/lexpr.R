new_lexpr <- function(x = list(), ...){
  if(!is.vector(x)) x <- list(x)
  stopifnot(all(are_expr(x)))
  class  <-  "lexpr"

  if(all(are_symbolic(x))){
    class <- c("lexpr_symbolic", class)
  }

  all_types <- c(callable = all(are_callable(x)),
                 symbolic = all(are_symbolic(x)),
                 syntactic_literal = all(are_syntactic_literal(x)))

  out <- structure(x, all_types = all_types, class = class)
  out
}


head_print_lexpr <- function(x, .as_list = FALSE){
  if(inherits(x, "lexprs_symbolic")) {
    type <- "symbolic_exprs"
  } else {
    type <- "exprs"
  }
  header <- sprintf("<list_of<%s>[%i]>", type, length(x))
  cat(header, "\n\n")
  invisible(header)
}

footer_print_lexpr <- function(x){
  stopifnot(inherits(x, "lexpr"))
  all_types <- attr(x, "all_types")
  out_types <- names(all_types)[all_types]
  n <- length(out_types)
  if(n > 0){
    out <- str_itemize(out_types)
    head <- format(c("All exprs are:  ", rep("", n - 1)))
    out <- paste0(head, out)
    sep <- strrep("_", max(nchar(out, "width")))
    footer <- c(sep, out)
    cat(footer, sep = "\n")
    invisible(footer)
  } else {
    invisible(NULL)
  }
}


body_print_lexpr <- function(x){
  n <- length(x)
  nms <- names(x)
  x <- names(lang_auto_name(x, .name_all = TRUE))
  names(x) <- nms

  pos <- are_unnamed(x)
  names(x)[pos] <- str_embrace(which(pos), "[[", "]]")

  fmt <- format(c(x, names(x)), justify = "left")
  fmt <- paste0(fmt, strrep(" ", 2L))
  out <- set_names(fmt[seq_len(n)], fmt[-seq_len(n)])
  # print(out)

  m_out <- matrix("", nrow = 2, ncol = n)
  m_out[1, ] <- unname(out)
  colnames(m_out) <- names(out)
  rownames(m_out) <- rep_len("", NROW(m_out))
  print(m_out, quote = FALSE)
}

print.lexpr <- function(x){
  head_print_lexpr(x)
  body_print_lexpr(x)
  footer_print_lexpr(x)
}

new_lexpr(rlang::exprs(mean(), b = var(), 1:10) |> rep(10))
