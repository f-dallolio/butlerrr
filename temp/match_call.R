cptr_error <- function(expr, alternative = NULL, silent = TRUE) {

  tryCatch(error = function(e) {
    list(result = alternative,
         error = conditionMessage(e),
         error_class = class(e))
    },
    list(result = expr, error = NULL))
}
safely <- function(.f, otherwise = NULL, quiet = TRUE) {
  .f <- as_function(.f)
  force(otherwise)
  stopifnot(is.logical(quiet) && length(quiet) == 1)
  function(...) purrr:::capture_error(.f(...), otherwise, quiet)
}

 possibly<- function(.f, otherwise = NULL, quiet = TRUE) {
  .f <- as_function(.f)
  force(otherwise)
  stopifnot(is.logical(quiet) && length(quiet) == 1)

  function(...) {
    tryCatch(.f(...), error = function(e) {
               otherwise
             })
  }
}

map(list(letters[1:3], 1:10), safely(sum))
cptr_error(sum("a"))




y <- letters
y <- setNames(y, rep("", length(y)))
names(y)[3] <- "c"

x <- list(1, "..." = as.list(y), c = NULL)
x
list_flatten_dots(x, .named = F)
purrr::list_flatten(x)

