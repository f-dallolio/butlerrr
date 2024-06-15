str_as_lang <- function(x, .strict = FALSE){
  if(!is_string(x)){
    if(.strict) stop("Input must be a single string.")
    return(x)
  }
  tryCatch(str2lang(x), error = function(e) as.symbol(x))
}

chr_as_lang <- function(x, .named = FALSE, .strict = FALSE, .simplify = FALSE){
  if(.simplify && length(x) == 1) {
    return(str_to_lang(x, .strict = .strict))
  }
  out <- lapply(x, str_as_lang, .strict = .strict)
  if(.named) names(out) <- x
  out
}




fn_as_lang <- function(x, .strict = FALSE, .simplify = FALSE, .no_ns = FALSE, .no_args = FALSE){
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
  out
}

fns_as_lang <- function(x, .no_ns = FALSE, .strict = FALSE, .simplify = FALSE){
  if(!is.vector(x)) x <- c(x)
  out <- lapply(x, fn_as_lang, .no_ns = .no_ns, .strict = .strict)
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
