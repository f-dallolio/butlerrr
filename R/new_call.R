#' Create new named calls
#'
#' @param .fn a string or a symbol.
#' @param .args a list.
#' @param .ns NULL, TRUE, or FALSE.
#' @param .private NULL, TRUE, or FALSE.
#'
#' @return a named call ("call_simple").
#' @name new-call
NULL
#' @export
#' @rdname new-call
new_call_simple <- function(.fn, .args = NULL, .ns = NULL, .private = NULL){

  if(!is.list(.args) && !is.null(.args)) .args <- list(.args)

  if(is.character(.fn)){ .fn <- str_as_lang(.fn) }

  if(!is.null(.ns) ){
    .ns <- switch (typeof(.ns),
                   character = str_as_lang(.ns),
                   symbol = .ns,
                   environment = str_as_lang(environmentName(.ns)),
                   stop(sprintf("The type of `.ns` must be \"string\",  \"environment\", or \"symbol\". Not \"%s\"",
                                typeof(.ns)),
                        call. = FALSE))
  } else {
    .ns <- NULL
  }

  if(!is.null(.private)){
    op <- if(.private) quote(`:::`) else quote(`::`)
  } else {
    if(is.null(.ns)) {
      op <- NULL
    } else {
      if(is_ns_sym(.fn)){
        op <- .fn[[1]]
      } else {
        op <- quote(`::`)
      }
    }
  }

  if(is_ns_sym(.fn)){
    .fn[[1]] <- op %||% .fn[[1]]
    .fn[[2]] <- .ns %||% .fn[[2]]
  } else {
    .fn <- c(op, .ns, .fn)
    if(length(.fn) == 3){
      .fn <- as.call(.fn)
    } else {
      .fn <- .fn
    }
  }

  as.call(c(.fn, .args))
}
#' @export
#' @rdname new-call
call_simple <- function(.fn, ..., .ns = NULL, .private = NULL){
  new_call_simple(.fn = .fn,
                  .args = list(...),
                  .ns = .ns,
                  .private = .private)
}


