str_as_lang <- function(x, ..., .out_type = NULL, .strict = FALSE){
  stopifnot("`...` must be empty." = ...length() == 0)
  if(!is_string(x)){
    if(.strict) stop("Input must be a single string.")
    return(x)
  }
  out <- tryCatch(str2lang(x), error = function(e) as.symbol(x))
  if(is.null(.out_type)) {
    return(out)
  }
  out <- as.list(out)
  choices <- c("sym", "syms", "symbol", "symbols", "name",
               "call", "calls", "language", "lang")
  out_type <- match.arg(.out_type, choices)
  if(out_type %in% c("sym", "syms", "symbol", "symbols", "name")){
    out[[1]]
  } else {
    as.call(out)
  }
}

chr_as_lang <- function(x, ..., .out_type = NULL, .named = FALSE, .strict = FALSE, .simplify = FALSE){
  stopifnot("`...` must be empty." = ...length() == 0)
  if(.simplify && length(x) == 1) {
    return(str_to_lang(x, .out_type = .out_type, .strict = .strict))
  }
  out <- lapply(x, str_as_lang, .out_type = .out_type, .strict = .strict)
  if(.named) names(out) <- x
  out
}




fn_as_lang <- function(x, .out_type = NULL, .strict = FALSE,
                       .simplify = FALSE, .no_ns = FALSE, .no_args = FALSE){
  if(!is.function(x)){
    if(.strict) stop(stop("Input must be a function."))
    return(x)
  }
  if(.simplify){
    .no_ns <- TRUE
    .no_args <- TRUE
  }

  out <- fn_call(x)

  if(.simplify){
    .no_ns <- TRUE
    .no_args <- TRUE}

  if(.no_ns) out[[1]] <- out[[1]][[3]]
  if(.no_args) out <- as.call(as.list(out[[1]]))

  if(is.null(.out_type)){ return(out) }

  choices <- c("sym", "syms", "symbol", "symbols", "name",
               "call", "calls", "language", "lang")
  out_type <- match.arg(.out_type, choices)
  if(out_type %in% c("sym", "syms", "symbol", "symbols", "name")){
    return(out[[1]])
  }
  out
}

fns_as_lang <- function(x, .no_ns = FALSE, .out_type = NULL, .named = FALSE, .strict = FALSE, .simplify = FALSE){
  if(!is.vector(x)) x <- c(x)
  out <- lapply(x, fn_as_lang, .no_ns = .no_ns, .out_type = .out_type, .strict = .strict)
  if(.named) names(out) <- vapply(out, \(x) deparse(as.list(x)[[1]]), character(1))
  if(.simplify && length(out) == 1) {
    return(out[[1]])
  }
  out
}

## environments

env_name_type <- function(x){
  stopifnot(is.environment(x))
  nm <- environmentName(x)
  if(isNamespace(x)) return(setNames(nm, "ns"))
  if(grepl("^package", nm)) return(setNames(nm, "pkg"))
  if(identical(x, globalenv())) return(setNames(nm, "global_env"))
  if(identical(x, emptyenv())) return(setNames(nm, "empty_env"))
  setNames(nm, "env")
}
