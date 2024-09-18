is_lambda_fn <- function(x){
  inherits(x, "function") && any(grepl("lambda", class(x)))
}
fn_eval <- function(.x, .f. ...){
  as_fn(.f)(.x, ...)
}
fn_exec <- function(.f, .x, ..., .named = TRUE){
  if(is.list(.f) || all(c(is.character(.f), length(.f) > 1))){
    out <- lapply(.f, fn_exec, .x = .x, ...)
    if(.named) {
      nms <- fn_name(.f)
      if(is.null(names(out))) {
        names(out) <- nms
      } else {
        no_nms <- names(out) == ""
        names(out)[no_nms] <- nms[no_nms]
      }
    }
    return(out)
  }
  fn <- as_fn(.f)
  if(is_lambda_fn(fn)){
    fn(.x)
  } else {
    fn(.x, ...)
  }
}
fn_exec(c(m = mean, sd), 1:10)

fn_name <- function(x){
  if(is.list(x) || all(c(is.character(x) && length(x) > 1))) {
    out <- vapply(x, fn_name, "")
    return(out)
  }
  x <- as_function(x)
  if (is_lambda(x)) {
    fn_bdy <- as.list(fn_body(x))
    fn_sym <- if(identical(fn_bdy[[1]], quote(`{`)))  fn_bdy[[2]][[1]] else fn_body[[1]]
    x <- eval(fn_sym, env = caller_env())
  }
  fns <- ns_get_fns(x, exports_only = TRUE)
  nms <- names(fns)
  out <- nms[vapply(fns, identical, logical(1), x)]

  if(length(out) == 0) {
    fns <- ns_get_fns(x, exports_only = FALSE)
    nms <- names(fns)
    out <- nms[vapply(fns, identical, logical(1), x)]
  }

  if(length(out) == 0) {
    stop("Cannot find funciton name")
  } else if (length(out) > 1) {
    return(out[[1]])
  }

  out
}


fn_env <- function(fn, ..., .named = TRUE) {
  if(is.list(fn) || all(c(is.character(fn) && length(fn) > 1))){
    out <- lapply(fn, fn_env)
    if(.named) {
      fn_nms <- fn_name()
      nms <- names(fn)
      if(is.null(fn_nms)) nms <- rep("", length(fn))
      no_nms <- nms == ""
      out[no_nms] <- fn_nms[no_nms]
    }
    return(out)
  }
  fn <- as_function(fn)
  if (is_lambda(fn)) {
    fn <- eval(fn_body(fn)[[1]], env = caller_env())
  }
  rlang::fn_env(fn)
}
