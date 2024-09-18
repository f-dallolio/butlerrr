#' Convert to function
#'
#' @description
#' `as_function2` is an extension of [rlang::as_function][rlang::as_function()] which transforms a one-sided formula into a function and
#' powers the lambda syntax.
#'
#' The main difference is that it also accepts as string input that includes namespace definition (e.g. `"pkg::fn"` or `"pkg:::fn"`).
#'
#' @param x A function, a formula, or a string.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function
#'   with up to two arguments: `.x` (single argument) or `.x` and `.y`
#'   (two arguments). The `.` placeholder can be used instead of `.x`.
#'   This allows you to create very compact anonymous functions (lambdas) with up
#'   to two inputs. Functions created from formulas have a special
#'   class. Use `is_lambda()` to test for it.
#'
#'   If a **string**, the function is looked up in `env`. Note that
#'   this interface is strictly for user convenience because of the
#'   scoping issues involved. Package developers should avoid
#'   supplying functions by name and instead supply them by value.
#'
#'   Unlike [rlang::as_function][rlang::as_function()], `as_function2()` also accepts a string input that includes namespace definition (e.g. `"pkg::fn"` or `"pkg:::fn"`).
#'   This is passed to [base::str2lang][base::str2lang()] and then evaluated in `env`.
#'
#'
#'
#' @param env Environment in which to fetch the function in case `x`
#'   is a string.
#' @inheritParams args_dots_empty
#' @inheritParams args_error_context
#' @export
#' @examples
#' f <- as_function(~ .x + 1)
#' f(10)
#'
#' g <- as_function(~ -1 * .)
#' g(4)
#'
#' h <- as_function(~ .x - .y)
#' h(6, 3)
#'
#' as_function("mean")
#' as_function2("mean")
#'
#' as_function("base::mean")
#' as_function2("base::mean")
#'
#' # Functions created from a formula have a special class:
#' is_lambda(f)
#' is_lambda(as_function(function() "foo"))
as_function2 <- function (x, env = global_env(), ..., arg = caller_arg(x), call = caller_env()) {
  if(is_string(x) && grepl(":", x)){
    x <- eval(str2lang(x), envir = env)
  }
  as_function(x, env = env, arg = arg, call = call)
}
