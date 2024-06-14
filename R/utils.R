caller_env <- function(n = 1) {
  parent.frame(n = n + 1)
}

caller_arg <- function(arg) {
  symbolic_to_str(substitute(arg))
}

as_function <- function(x, env = globalenv()) {
  if (is.function(x)) {
    return(x)
  }
  if (inherits(x, "formula")) {
    if (length(x) > 2) {
      stop("`x` must be a right-sided formula.")
    }
    args <- list(
      ... = quote(expr = ),
      .x = quote(..1), .y = quote(..2), . = quote(..1)
    )
    fn <- as.function(c(args, as.list(x)[[2]]))
    return(fn)
  }
  if (is.character(x) && length(x) == 1) {
    return(get(x, envir = env, mode = "function"))
  }
  stop("Canno convert input to a function")
}

inside_fn <- function() {
  !identical(parent.frame(), globalenv())
}

if_null <- function(x, y){
  if(is.null(x)){
    y
  } else {
    x
  }
}

unnest_call <- function(x){
  print(x)
  xx <- as.list(x)
  if(length(xx) > 1){
    xx <- xx[[2]]
  }
  if(is_call2(xx)){
    x <- xx
  } else {
    return(x)
  }
  invisible(unnest_call(x))
}




