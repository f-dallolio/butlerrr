#' Function Formals
#'
#' @param .fn a function.
#' @param .unname_dots logical.
#' @param ... named objects defining the new formals defaults.
#'
#' @return a list ([fn_fmls] and [fn_fmls_syms]), a character vector ([fn_fmls_nms]), or a function([fn_fmls_update]).
#' @name fn-fmls
NULL
#'
#' @rdname fn-fmls
#' @export
fn_fmls <- function(.fn){
  type <- typeof(.fn)
  if(type == "special") return(NULL)
  fmls <- formals(.fn)
  if(is.null(fmls)) fmls <- list(x = quote(expr = ))
  if(is.pairlist(fmls)) return(as.list(fmls))
  fmls
}
#'
#' @rdname fn-fmls
#' @export
fn_fmls_nms <- function(.fn){
  type <- typeof(.fn)
  if(type == "special") return(NULL)
  fmls <- formals(.fn)
  if(is.null(fmls)) return("x")
  names(fmls)
}
#'
#' @rdname fn-fmls
#' @export
fn_fmls_syms <- function(.fn, .unname_dots = FALSE){
  type <- typeof(.fn)
  if(type == "special") return(NULL)
  fmls <- formals(.fn)
  if(is.null(fmls)) fmls <- list(x = quote(expr = ))
  if(is.pairlist(fmls)) fmls <- as.list(fmls)
  fmls_nms <- set_names(names(fmls))
  out <- lapply(fmls_nms, str_as_lang)
  if(.unname_dots) names(out)[names(out) == "..."] <- ""
  out
}
#'
#' @rdname fn-fmls
#' @export
fn_fmls_update <- function(.fn, ...){

  call <- match.call()
  head <- call$.fn
  args <- list(...)
  args_nms <- names(args)


  fmls <- formals(.fn)
  fmls_nms <- names(fmls)
  has_dots <- "..." %in% fmls_nms

  body_call_args <- fn_fmls_syms(.fn, .unname_dots = TRUE)
  fn_nms <- args_nms[args_nms %in% fmls_nms]
  body_call_args[fn_nms] <- args[fn_nms]


  new_fmls <- fmls[setdiff(fmls_nms, args_nms)]

  dots_nms <- setdiff(args_nms, fmls_nms)
  flag <- has_dots && length(dots_nms)
  if(flag){
    i <- which(names(body_call_args) == "")
    dots_args <- args[args_nms == dots_nms]
    body_call_args <- c(body_call_args[seq2(1, i - 1)],
                        c(dots_args, body_call_args[[i]]),
                        body_call_args[seq2(i + 1, length(body_call_args))])
  }
  body_call <- as.call(c(head, body_call_args))

  fn_out <- as.function(c(new_fmls, body_call))
  environment(fn_out) <- parent.frame()
  fn_out

}
