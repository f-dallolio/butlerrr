#' Convert to Function
#'
#' This function converts a one-sided formula into a function. It is a
#' simplified version of [rlang::as_function][rlang::as_function].
#'
#' @param .f A one-sided formula or a function. If `.str = TRUE`, it also
#'   accepts a string.
#' @param ... Must remain empty.
#' @param .str If `TRUE` (default), `.f` can also be a string. If `FALSE` and
#'   `.f` is not a one-sided formula or a function, it returns its input.
#' @param .env Only for `.str = TRUE`. The environment where to get the
#'   function.

#'
#' @return A function.
#' @export
as_fn <- function(.f, ..., .env = .GlobalEnv){
  stopifnot(...length() == 0)
  if (inherits(.f, "formula")) {
    .f <- as.list(.f)
    fn <- function(..., .x = ..1, .y = ..2, . = ..1){}
    body(fn) <- .f[[length(.f)]]
    environment(fn) <- environment(.f)
    class(fn) <- c("rlang_lambda_function", class(fn))
    return(fn)
  }
  if(is.function(.f)) return(.f)
  if(is.character(.f) && length(.f) == 1){
    out <- get(.f, envir = .env, mode = "function")
    return(out)
  }
  stop("`.f` must be a formula, a function, or a string.")
}
#
# fn_eval <- function(.x, .f, ..., .env = .GlobalEnv) {
#   as_fn(.f, .env = .env)(.x, ...)
# }
#
#
#
# fn_ns_name(c("mean", "sd"))
#
#
#
#
#
# fn_exec <- function(.f, ..., .env = .GlobalEnv) {
#   if(is.list(.f) && length(.f) > 1) {
#     out <- lapply(.f, fn_exec, ..., .env = .env)
#     fn_names = vapply(out, attr, "", "fn_names")
#     names(out) <- fn_names
#     out <- lapply(out, `attr<-`, "fn_names", NULL)
#     return(out)
#   }
#   if(is.character(.f)) {
#     .f <- get(.f, envir = .env, mode = "function")
#   }
#   fn_names <-  fn_name(.f, ns_rm = FALSE)
#   structure(.f(...),
#             fn_names = fn_names)
# }
#
# fn_exec_list <- function(.f, ..., .env = .GlobalEnv) {
#   if(is.character(.f)) {
#     .f <- get(.f, envir = .env, mode = "function")
#     nm <- .f
#   } else if (is.function(.f)) {
#     nm <- fn_name(.f)
#   } else {
#     stop("`.f` must be a string or a function.")
#   }
#   .f(...)
# }
#
# fn_exec |>  environment() |> isNamespace()
# fn_exec(c(scale, mean), 1:10)
# formals(as_fn(~mean(.x)))  |>  modifyList(list(scale = TRUE, .x = 1:10, center = FALSE))
