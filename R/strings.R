#' Padding of character vector of integers
#'
#' @param x a numeric vector coercible to integer or it character equivalent.
#' @param pad string. Defaults to "0". The padding on the left side.
#'
#' @return a character vector.
#' @examples
#' x <- c(1, 21, 301, 401)
#' str_pad_num(x)
#' @export
str_pad_num <- function(x, pad = "0") {
  stopifnot(is.numeric(x) || is_numeric_chr(x))
  if (!is.character(x)) x <- as.character(x)
  width <- max(stringr::str_width(x))
  stringr::str_pad(
    string = x,
    width = width,
    side = "left",
    pad = pad,
    use_width = TRUE
  )
}



#' String formatiing/padding
#'
#' @param x a character vector or a string.
#' @param width integer. If NULL (default), it is automatically set equal to the
#'   max width among all elements of x.
#' @param justify one of c("left", "right", "centre", "none").
#' @param ellipsis string. It indicates where the string has been truncated.
#' @param ... other arguments passed to base::format.
#'
#' @return character representations of the elements of x in a common format.
#'   THat is, aappropriately padded.
#' @examples
#' x <- gsub("\\s", "_", rownames(mtcars))
#' str_format(x) #' default: justify - "left
#' str_format(x, justify = "right")
#' str_format(x, justify = "centre")
#' str_format(x, width = 6)
#' str_format(x, width = 6, ellipsis = "**")
#'
#' @export
str_format <- function(x,
                       width = NULL,
                       justify = c("left", "right", "centre", "none"),
                       ellipsis = "...",
                       ...) {
  max_wdt <- max(stringr::str_width(x))

  if (is.null(width)) {
    width <- max_wdt
  }

  if (width < max_wdt) {
    x <- stringr::str_trunc(
      string = x,
      width = width,
      side = "right",
      ellipsis = ellipsis
    )
  }
  format(x, justify = justify, width = width, ...)
}


#' Enumerate and format the elements of a character vector.
#'
#' @param x a character vector.
#' @param sep a string. For [str_enum] it indicates the separator betweem item numbers and
#'   strings. For, [str_itemize], it represents the item indicator.
#' @param pad_num TRUE or FALSE. If TRUE (default), numbers are padded with
#'   str_pad_num.
#' @param format_str TRUE or FALSE. If TRUE (default), strings are formatted
#'   with str_format.
#'
#' @return a character vector.
#' @name string-itemize
NULL
#' @examples
#' x <- gsub("\\s", "_", rownames(mtcars))[1:12]
#' str_enum(x)
#' # different separator
#' str_enum(x, sep = "- ")
#' # numbers are not padded if `pad_num` is FALSE
#' str_enum(x, pad_num = FALSE)
#' # strings not of same width if `format_str` is FALSE
#' str_enum(x, format_str = FALSE)
#'
#' item1 <- str_itemize(x)
#' # identical to...
#' item2 <- str_itemize(x, type = "list", item = "-", format_str = TRUE)
#' item1
#' item2
#'
#' # str_itemize is perfect for `cat`:
#' cat(item1, sep = "\n")
#' @rdname string-itemize
#' @export
str_enum <- function(x, sep = ": ", pad_num = TRUE, format_str = TRUE) {
  enum <- seq_along(x)
  if (pad_num) enum <- str_pad_num(enum)
  if (format_str) x <- str_format(x)
  paste0(enum, sep, x)
}
#' @rdname string-itemize
#' @export
str_itemize <- function(x, type = c("list", "enum", "letters", "up_letters"),
                        sep = "-", format_str = TRUE, ...) {
  sep <- gsub("^\\s*|\\s*$", "", sep)

  type <- match.arg(type)
  if (type == "enum") {
    return(str_enum(x, format_str = format_str, sep = sep))
  }

  if (format_str) {
    x <- str_format(x)
  }

  if (type %in% c("letters", "up_letters")) {
    n <- length(x)
    n2 <- length(letters)
    n_times <- ceiling(n / n2)
    nn <- rep_each(seq_len(n_times), n2)

    if (type == "letters") pre <- letters else pre <- LETTERS
    pre <- strrep(pre, nn)[1:n]

    if (format_str) {
      pre <- str_format(pre, justify = "right")
    }

    return(paste(pre, sep, x))
  }

  paste(sep, x)
}

#' Collapse Strings
#'
#' @inheritParams str_embrace
#' @param x a character vector.
#' @param collapse defaults to `.c`.
#' @param recycle0 	logical indicating if zero-length character arguments should
#'   result in the zero-length `character(0)`. When `collapse` is a string,
#'   `recycle0` does not recycle to zero-length, but to `""`.
#' @param .trailing TRUE or FALSE. If TRUE (default), there is no return
#'   (`"\n"`) at the end of the output. If FALSE, a return (`"\n"`) is pasted at
#'   the end of the output.
#'
#' @return a string.
#' @examples
#' str_collapse(letters)
#' str_collapse(letters, .c = str_oxford)
#' @name string-collapse
NULL
#' @export
#' @name string-collapse
str_collapse <- function(x, .c = "", ..., collapse = .c, recycle0 = FALSE) {
  if (is.character(collapse) && length(collapse) == 1) {
    paste(x, collapse = collapse, recycle0 = recycle0)
  } else {
    fn <- as_function(collapse)
    fn(x, ...)
  }
}
#' @export
#' @name string-collapse
str_coll <- str_collapse
#' @export
#' @name string-collapse
str_p0 <- function(x, .c = "", ..., collapse = .c, recycle0 = FALSE) {
  str_collapse(x, .c = .c, ..., collapse = collapse, recycle0 = recycle0)
}
#' @export
#' @name string-collapse
str_p <- function(x, .c = " ", ..., collapse = .c, recycle0 = FALSE) {
  str_collapse(x, .c = .c, ..., collapse = collapse, recycle0 = recycle0)
}


#' Concatenate strings with `"\n"`.
#'
#' @param ... strings or R objects coercible to strings.
#' @param .f a function or a formula. It uses a simplified version of
#'   [rlang::as_function].
#' @param .args list with extra arguments for `.f`.
#' @param .trailing TRUE or FALSE. If TRUE (default), a return (`"\n"`) is
#'   pasted at the end of the output string.
#'
#' @return a string made of input strings separated by returns (`"\n"`).
#' @examples
#' out1 <- str_line(letters[1:4])
#' out1
#' out2 <- str_line(letters[1:4], .f = str_parens, .args = list(type = "angle"))
#' out2
#' out3 <- str_line(letters[1:4], .f = str_parens, .trailing = TRUE)
#' out3
#' cat_line0(out1, "else")
#' cat_line0(out2, "else")
#' cat_line0(out3, "else")
#' @name string-line
NULL
#' @rdname string-line
#' @export
str_return <- function(..., .f = NULL, .args = NULL, .trailing = FALSE) {
  x <- as.character(c(...))
  stopifnot(is.character(x))
  if (!is.null(.f)) {
    if (is.character(.f) && length(.f) == 1) {
      x <- paste0(x, collapse = .f)
    } else {
      fn <- as_function(.f)
      x <- do.call(fn, append(list(x), .args))
    }
  }
  if (.trailing) {
    paste0(x, "\n", collapse = "")
  } else {
    paste(x, collapse = "\n")
  }
}
#' @rdname string-line
#' @export
str_r <- str_return
#' @rdname string-line
#' @export
cat_return <- function(..., .f = NULL, .args = NULL) {
  x <- c(...)
  if (!is.null(.f)) {
    fn <- as_function(.f)
    x <- do.call(fn, append(list(x), .args))
  }
  cat(x, sep = "\n")
}
#' @rdname string-line
#' @export
rcat <- cat_return

#' Embrace Strings
#'
#' @param x a character vector or an object coercible to one.
#' @param left,right a string.
#' @param type one of `c("round", "squared", "curly", "angle")`. The type of
#'   parentheses.
#' @param .c collapser. A string, a function, or a formula. It defines how
#'   strings in the vector are collapsed. If NULL (default), the embracing gets
#'   applied to each element of the input.
#' @param ... optional arguments of `.c`.
#'
#' @return a string or a character vector.
#' @examples
#' x <- gsub("\\s", "_", rownames(mtcars))[1:12]
#' x
#' str_embrace(x, left = "-")
#' str_embrace(x, left = "-", right = "--")
#' str_embrace(x, left = "-", .c = str_oxford)
#' str_parens(x)
#' str_parens(x, "round")
#' str_parens(x, "round", .c = str_oxford)
#' str_parens(x, "squared")
#' str_parens(x, "curly")
#' str_parens(x, "angle")
#' @name string-embrace
NULL
#'
#' @rdname string-embrace
#' @export
str_embrace <- function(x,
                        left,
                        right = left,
                        .c = NULL,
                        ...) {
  stopifnot(is.character(left) && is.character(right))
  if (!is.null(.c)) {
    if (is.character(.c)) {
      stopifnot(
        "If `c` has class \"character\", it should be of lenght 1." =
          length(.c) == 1
      )
      x <- paste(x, collapse = .c)
    } else if (is.function(.c) || inherits(.c, "formula")) {
      x <- rlang::as_function(.c)(x, ...)
    } else {
      msg0 <- "`c` should be a single string, a function, or a formula."
      msg <- paste0(msg0, " Not an object of class ", class(.c), ".")
      stop(msg)
    }
  }
  paste0(left, x, right)
}
#'
#' @rdname string-embrace
#' @export
str_parens <- function(x,
                       type = c("round", "squared", "curly", "angle"),
                       .c = NULL,
                       ...) {
  type <- match.arg(type)
  switch(type,
    "round" = str_embrace(x, left = "(", right = ")", .c = .c, ...),
    "squared" = str_embrace(x, left = "[", right = "]", .c = .c, ...),
    "curly" = str_embrace(x, left = "{", right = "}", .c = .c, ...),
    "angle" = str_embrace(x, left = "<", right = ">", .c = .c, ...)
  )
}


#' Collapse a character vector using Oxford Comma
#'
#' @param x a character vector.
#' @param or TRUE or FALSE. If TRUE, the last element is coillapsed using 'or'.
#'   If FALSE (default), the character is collapsed using 'and'.
#'
#' @return a string.
#' @examples
#' str_oxford(letters[1:2])
#'
#' str_oxford(letters[1:4])
#'
#' str_oxford(letters[1:4], or = TRUE)
#'
#' @export
str_oxford <- function(x, or = FALSE) {
  if (or) {
    stringr::str_flatten_comma(x, last = ", or ")
  } else {
    stringr::str_flatten_comma(x, last = ", and ")
  }
}
strsplit

str_sep <- function (x, split, sel = NULL, fixed = FALSE, perl = FALSE, useBytes = FALSE) {
  out <- strsplit(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)
  n <- length(out)
  if(is.null(sel)){
    if(n == 1) return(out[[1]])
    return(out)
  }
  out <- lapply(out, get_elements, pos = sel)
  if(n == 1) return(out[[1]])
  nn <- lengths(out)
  if(all(nn <= 1)){
    out[nn == 0] <- NA_character_
    return(unlist(out))
  }
  out
}

get_elements <- function(x, pos, pos_out = FALSE, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, strict = FALSE, warn = TRUE){
  if(is_string(pos)){
    pos <- grep(pattern = pos,
                 x = x,
                 ignore.case = ignore.case,
                 perl = perl,
                 fixed = fixed,
                 useBytes = useBytes)
  } else {
    pos <- as.integer(pos)
  }
  if(length(pos) == 0) pos <- 0
  n <- length(x)
  flag <- max(pos) > n
  if(flag){
    msg <- sprintf("`pos` is larger than the length of `x` (%i)", n)
    if(strict) stop(msg)
    if(warn) cat("Warning:", msg, "\n")
  }
  pos <- pos[as.integer(pos) <= n]
  out <- x[pos]
  if(pos_out) {
    out <- rep_along(FALSE, x)
    out[pos] <- TRUE
  }
  out
}

flag_elements <- function(x, .p, ..., .names = FALSE, .strict = FALSE){
  if(.names){
    if(.strict) stopifnot("Input is not named" = !is.null(names(x)))
    if(is.null(names(x))) return(NULL)
    x <- names(x)
  }
  if(is.character(.p) || is.numeric(.p)){
    .f <- function(x, ...) get_elements(x, pos = .p, pos_out = TRUE)
  } else {
    .f <- as_function(.p)
  }
  if(is.list(x)){
    pos <- vapply(x, .f, logical(1), ...)
  } else {
    pos <- .f(x)
  }
  pos
}

flag_names <- function(x, .p, ...){
  flag_elements(x, .p, ..., .names = TRUE)
}

which_elements <- function(x, .p, ..., .names = FALSE, .strict = FALSE){
  pos <- flag_elements(x = x, .p = .p, ..., .names = .names, .strict = .strict)
  which(pos)
}

which_named <- function(x, .p, ...){
  which_elements(x, .p, ..., .names = TRUE)
}

keep_elements <- function(x, .p, ..., .names = FALSE, .strict = FALSE){
  pos <- flag_elements(x = x, .p = .p, ..., .names = .names, .strict = .strict)
  x[pos]
}

keep_named <- function(x, .p, ...){
  keep_elements(x, .p, ..., .names = TRUE)
}

discard_elements <- function(x, .p, ..., .names =FALSE){
  pos <- flag_elements(x = x, .p = .p, ..., .names = .names, .strict = .strict)
  x[!pos]
}

discard_named <- function(x, .p, ...){
  discard_elements(x, .p, ..., .names = TRUE)
}

#' Convert Strings to Symbolic Objects (generalizes [base::str2lang])
#'
#' @param x a string or a character vector.
#' @param strict TRUE or FALSE (default). If TRUE, only strings are accepted as
#'   input. Other objects will throw an error even if coercible to
#'   strings/character.
#'
#' @return a symbolic object or a lsit of symbolic objects.
#' @examples
#' str_to_symbolic("mean")
#' str_to_symbolic("mean(1:10)")
#' str_to_symbolic("+")
#' chr_to_symbolic(c("a", "c", "d", "e"))
#' @name string-to-symbolic
NULL
#' @rdname string-to-symbolic
#' @export
str_to_symbolic <- function(x, strict = FALSE) {
  if (!is.character(x)) {
    if (strict) {
      stop("Input is not a string")
    }
    return(x)
  }
  out <- tryCatch(
    str2lang(x),
    error = function(e) as.symbol(x)
  )
  if (rlang::is_syntactic_literal(out)) {
    return(as.symbol(out))
  }
  out
}
#' @rdname string-to-symbolic
#' @export
str2symc <- str_to_symbolic
#'
#' @rdname string-to-symbolic
#' @export
chr_to_symbolic <- function(x, strict = FALSE) {
  if (rlang::is_symbolic(x)) {
    str_to_symbolic(x)
  } else {
    lapply(x, str_to_symbolic)
  }
}
#' @rdname string-to-symbolic
#' @export
chr2symc <- chr_to_symbolic



str_as_vector <- function(x){
  .f <- function(x){
    n <- length(x)
    if(n == 1) return(as.character(x))
    sprintf("c(%s)", paste(x, collapse = ", "))
  }
  if(is.list(x) && length(x) > 1){
    out <- vapply(x, .f, character(1))
    setNames(out, names(x))
  } else {
    .str_as_vector(unlist(x))
  }
}
