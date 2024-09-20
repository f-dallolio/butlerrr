fn_body(as_function2) |> is_call(name = "{")


fn_body_simple <- function (fn = caller_fn()) {
  stopifnot(is_closure(fn))
  body <- fn_body(fn)
  body2 <- call_args(body)
  if(is_lambda(fn) || length(body2) == 1) {
    body2[[1]]
  } else {
    body
  }
}

fn_get_lambda <- function(fn = caller_fn(), str_out = FALSE){
  stopifnot(is_lambda(fn))
  out <- call("~", fn_body_simple(fn))
  if(str_out) {
    as_label(out)
  } else {
    out
  }
}
fn <- as_function(~mean(.x))
fn_get_lambda(fn, str_out = T)

fn_list <- function(x, .named = FALSE, .names_fn = c("fn_names", "fn_names0", "fn_names2")){
  if(!is_list(x)) x <- list(x)
  fns <- lapply(x, as_closure2, env = caller_env())
  if(!length(fns)){
    fn <- new_function(
      args = pairlist2(x = quote(expr = ), ... = quote(expr = )),
      body = quote(x) )
    fns <- list(fn)
  }
  if(!.named) return(fns)

  .names_fn <- match.arg(.names_fn)
  fn_nms <- as_function2(.names_fn)
  names(fns) <- do.call(fn_nms, fns)
  fns
}

fn_list(c(mean, ~mean(.x), "mean"), .named = TRUE)



fn_c <- function (..., .dir = c("backward", "forward")) {
  .dir <- match.arg(.dir, c("backward", "forward"))
  fns <- fn_list(list2(...))
  if (.dir == "backward") {
    fns <- rev(fns)
  }
  fns
}

fn_call <- function(f, ...) f(...)

fn_list <- function(..., dir = c("backward", "forward")){
  .dir <- match.arg(.dir, c("backward", "forward"))
  fns <- lapply(list2(...), as_function2, env = caller_env())
  if(.dir == "backward") {
    n <- length(fns)
    first_fn <- fns[[n]]
    fns <- rev(fns[-n])
  } else {
    first_fn <- fns[[1]]
    fns <- fns[-1]
  }
}

f_call <- function(f,...) (f, ...)
f_call(mean, var)
purrr::compose(eval, as.call, as.list) |> lapply(exec)

`Reduce(fun_call, )
.dir <- "backward"
x <- list(mean, ~mean(.x), "base::mean")
fnc <-function (..., .dir = c("backward", "forward")) {
  x <- list2(...)
  fns <- lapply(x, as_closure2, env = caller_env())
  .dir <- match.arg(.dir)
  purrr::compose(splice(fns), .dir = .dir)
}
x1 <- fnc(exp, log, mean, ~ scale(.x, center = FALSE))(1:100)
exp(log(mean(scale(1:100, center = FALSE))))

purrr::partial(mean, na.rm = TRUE)
fnr <- function()
