#' Padding of character vector of integers
#'
#' @param x a numeric vector coercible to integer or it character equivalent.
#' @param pad string. Defaults to "0". The padding on the left side.
#'
#' @return a character vector.
#' @export
str_pad_num <- function(x, pad = "0"){
  stopifnot(is.numeric(x) || is_numeric_chr(x))
  if(!is.character(x)) x <- as.character(x)
  width <- max(stringr::str_width(x))
  stringr::str_pad(string = x,
                   width = width,
                   side = "left",
                   pad = pad,
                   use_width = TRUE)
}



#' String formatiing/padding
#'
#' @param x a character vector or a string.
#' @param width integer. If NULL (default), it is automatically set equal to the max width among all elements of x.
#' @param justify one of c("left", "right", "centre", "none").
#' @param ellipsis string. It indicates where the string has been truncated.
#' @param ... other arguments passed to base::format.
#'
#' @return character representations of the elements of x in a common format. THat is, aappropriately padded.
#' @export
str_format <- function(x,
                       width = NULL,
                       justify = c("left", "right", "centre", "none"),
                       ellipsis = "...",
                       ...){
  max_wdt <- max(stringr::str_width(x))

  if(is.null(width)){
    width <-  max_wdt
  }

  if(width < max_wdt){
    x <- stringr::str_trunc(string = x,
                              width = width,
                              side = "right",
                              ellipsis = ellipsis)
  }
  format(x, justify = justify, width = width, ...)
}


#' Enumerate and format the elements of a character vector.
#'
#' @param x a character vector.
#' @param sep a string indicating the separation betweem item numbers and strings.
#' @param pad_num TRUE or FALSE. If TRUE (default), numbers are padded with str_pad_num.
#' @param format_str TRUE or FALSE. If TRUE (default), strings are formatted with str_format.
#'
#' @return a character vector.
#' @export
str_enum <- function(x, sep = ": ", pad_num = TRUE, format_str = TRUE){
  enum <- seq_along(x)
  if(pad_num) enum <- str_pad_num(enum)
  if(format_str) x <- str_format(x)
  paste0(enum, sep, x)
}



#' Embrace Strings
#'
#' @param x a character vector or an object coercible to one.
#' @param left,right a string.
#' @param type one of `c("round", "squared", "curly", "angle")`. The type of parentheses.
#' @param .c collapser. A string, a function, or a formula. It defines how strings in the vector are collapsed. If NULL (default), the embracing gets applied to each element of the input.
#' @param ... optional arguments of `.c`.
#'
#' @return a string or a character vector.
#' @name string-embrace
NULL
#'
#' @rdname string-embrace
#' @export
str_embrace <- function(x,
                        left,
                        right = left,
                        .c = NULL,
                        ...){
  stopifnot(is.character(left) && is.character(right))
  if(!is.null(.c)){
    if(is.character(.c)) {
      stopifnot(
        "If `c` has class \"character\", it should be of lenght 1." =
          length(.c) == 1
      )
      x <- paste(x, collapse = .c)
    } else if(is.function(.c) || inherits(.c, "formula")){
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
                       ...){
  type <- match.arg(type)
  switch(type,
         "round" = str_embrace(x, left = "(", right = ")", .c = .c, ...),
         "squared" = str_embrace(x, left = "[", right = "]", .c = .c, ...),
         "curly" = str_embrace(x, left = "{", right = "}", .c = .c, ...),
         "angle" = str_embrace(x, left = "<", right = ">", .c = .c, ...))
}


#' Collapse a character vector using Oxford Comma
#'
#' @param x a character vector.
#' @param or TRUE or FALSE. If TRUE, the last element is coillapsed using 'or'. If FALSE (default), the character is collapsed using 'and'.
#'
#' @return a string.
#' @examples
#' str_oxford(letters[1 : 2])
#' str_oxford(letters[1 : 4])
#' str_oxford(letters[1 : 4], or = TRUE)
#'
#' @export
str_oxford <- function(x, or = FALSE){
  if(or){
    stringr::str_flatten_comma(x, last = ", or ")
  } else {
    stringr::str_flatten_comma(x, last = ", and ")
  }
}




#' Convert Strings to Symbolic Objects (generalizes [base::str2lang])
#'
#' @param x a string or a character vector.
#' @param strict TRUE or FALSE (default). If TRUE, only strings are accepted as input. Other objects will throw an error even if coercible to strings/character.
#'
#' @return a symbolic object or a lsit of symbolic objects.
#' @examples
#' str_to_symbolic("mean")
#' str_to_symbolic("mean(1:10)")
#' str_to_symbolic("+")
#' chr_to_symbolic(c("a", "c", "d", "e"))
#' @name string-to-symbolic
NULL
#'
#' @rdname string-to-symbolic
#' @export
str_to_symbolic <- function(x, strict = FALSE){
  if(!is.character(x)){
    if(strict){
      stop("Input is not a string")
    }
    return(x)
  }
  out <- tryCatch(
    str2lang(x),
    error = function(e) as.symbol(x)
  )
  if( rlang::is_syntactic_literal(out) ){
    return(as.symbol(out))
  }
  out
}
#'
#' @rdname string-to-symbolic
#' @export
chr_to_symbolic <- function(x, strict = FALSE){
  if(rlang::is_symbolic(x)){
    str_to_symbolic(x)
  } else {
    lapply(x, str_to_symbolic)
  }
}
