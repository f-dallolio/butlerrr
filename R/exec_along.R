#
# map <- function(.x,
#                 .f,
#                 ...,
#                 .named = FALSE,
#                 .strict = FALSE){
#   purrr_attached <- is_pkg_attached("purrr")
#   if(purrr_attached){
#     fn <- get0("map", envir = asNamespace("purrr"), mode = "function")
#     fn(.x, .f, ...)
#   } else {
#     # exec_along
#     exec_along(.x = .x,
#                .f = .f,
#                ...,
#                .named = .named,
#                .strict = .strict,
#                .type_out = NULL)
#   }
# }
#
# map_lgl <- function(.x,
#                 .f,
#                 ...,
#                 .named = FALSE,
#                 .strict = FALSE){
#   purrr_attached <- is_pkg_attached("purrr")
#   if(purrr_attached){
#     fn <- get0("map_lgl", envir = asNamespace("purrr"), mode = "function")
#     fn(.x, .f, ...)
#   } else {
#     exec_along(.x = .x,
#                .f = .f,
#                ...,
#                .named = .named,
#                .strict = .strict,
#                .type_out = "logical")
#   }
# }
#
# map_int <- function(.x,
#                 .f,
#                 ...,
#                 .named = FALSE,
#                 .strict = FALSE){
#   purrr_attached <- is_pkg_attached("purrr")
#   if(purrr_attached){
#     fn <- get0("map_int", envir = asNamespace("purrr"), mode = "function")
#     fn(.x, .f, ...)
#   } else {
#     exec_along(.x = .x,
#                .f = .f,
#                ...,
#                .named = .named,
#                .strict = .strict,
#                .type_out = "int")
#   }
# }
#
# map_dbl <- function(.x,
#                 .f,
#                 ...,
#                 .named = FALSE,
#                 .strict = FALSE){
#   purrr_attached <- is_pkg_attached("purrr")
#   if(purrr_attached){
#     fn <- get0("map_dbl", envir = asNamespace("purrr"), mode = "function")
#     fn(.x, .f, ...)
#   } else {
#     exec_along(.x = .x,
#                .f = .f,
#                ...,
#                .named = .named,
#                .strict = .strict,
#                .type_out = "double")
#   }
# }
#
# map_cpl <- function(.x,
#                 .f,
#                 ...,
#                 .named = FALSE,
#                 .strict = FALSE){
#   exec_along(.x = .x,
#              .f = .f,
#              ...,
#              .named = .named,
#              .strict = .strict,
#              .type_out = "complex")
# }
#
# map_chr <- function(.x,
#                 .f,
#                 ...,
#                 .named = FALSE,
#                 .strict = FALSE){
#   purrr_attached <- is_pkg_attached("purrr")
#   if(purrr_attached){
#     fn <- get0("map_chr", envir = asNamespace("purrr"), mode = "function")
#     fn(.x, .f, ...)
#   } else {
#     exec_along(.x = .x,
#                .f = .f,
#                ...,
#                .named = .named,
#                .strict = .strict,
#                .type_out = "character")
#   }
# }
#
# map_raw <- function(.x,
#                 .f,
#                 ...,
#                 .named = FALSE,
#                 .strict = FALSE){
#   purrr_attached <- is_pkg_attached("purrr")
#   if(purrr_attached){
#     fn <- get0("map_raw", envir = asNamespace("purrr"), mode = "function")
#     fn(.x, .f, ...)
#   } else {
#     exec_along(.x = .x,
#                .f = .f,
#                ...,
#                .named = .named,
#                .strict = .strict,
#                .type_out = "raw")
#   }
# }
#
#
# vapply_output <- function(x,
#                           type = c("logical",
#                                    "integer",
#                                    "double",
#                                    "complex",
#                                    "character",
#                                    "raw")){
#   type <- match.arg(arg = x, choices = type)
#   switch(type,
#          logical = logical(1),
#          integer = integer(1),
#          double = double(1),
#          complex = complex(1),
#          character = character(1),
#          raw = raw(1))
# }
#
# is_pkg_attached <- function(pkg_name) {
#   stopifnot(is_string(pkg_name))
#   paste0("package:", pkg_name) %in% search()
# }
#
# exec_along <- function(.x,
#                        .f,
#                        ...,
#                        .named = FALSE,
#                        .strict = FALSE,
#                        .type_out = NULL){
#   nms <- as.list(substitute(.x))
#   if(length(nms) == 1) {
#     nms <- lang_to_str(nms)
#   } else {
#     nms <- lang_to_chr(as.list(nms)[-1])
#   }
#   if(!is.vector(.x)) x <- list(.x)
#   .fn <- function(i, x = .x, ..., .strict = .strict) {
#     out <- try(as_function(.f)(x[[i]], ...), silent = TRUE)
#     if(inherits(out, "try-error")){
#       if(.strict){
#         msg <- sprintf("Element %i threw an error.", i)
#         stop(msg)
#       }
#       return(NULL)
#     }
#     out
#   }
#   if(is.null(.type_out)){
#     out <- lapply(seq_along(.x), .fn, .x, ..., .strict = .strict)
#   } else {
#     out <- vapply(seq_along(.x), .fn, vapply_out(.type_out), .x, ..., .strict = .strict)
#   }
#   if(.named){
#     nms_out <- names(out)
#     if(is.null(nms_out)) nms_out <- rep_along(x = "", along = out)
#     flags <- nms_out == ""
#     names(out)[flags] <- nms[flags]
#   }
#   out
# }
