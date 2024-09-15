#' Get Namespace Functions
#'
#' @param x A function, a string, or an environment.
#' @param exports_only If `TRUE` (default), it returns only the exported functions. If `FALSE`, the private ones are included too.
#'
#' @return A named list of functions.
#' @export
#'
#' @examples
#' ns_get_fns(mean)
#' ns_get_fns(environment(mean))
#'
#' export_fns <- ns_get_fns("base")
#' all_fns <- ns_get_fns("base", exports_only = FALSE)
#' length(export_fns)
#' length(all)

ns_get_fns <- function(x, exports_only = NULL){
  if(is.function(x)) x <- tryCatch(environment(x), error = function(e) asNamespace("base"))
  if(is.character(x)) x <- asNamespace(x)
  if (is.null(exports_only) || !isNamespace(x)) {
    nms <- names(x)
  } else if (exports_only) {
    nms <- getNamespaceExports(x)
  } else if (!exports_only) {
    nms <- setdiff(names(x), getNamespaceExports(x))
  } else {
    stop("`export_only` must be NULL, TRUE, or FALSE")
  }
  out <- mget(nms, envir = x, "function", ifnotfound = list(NULL))
  unlist(out)
}
