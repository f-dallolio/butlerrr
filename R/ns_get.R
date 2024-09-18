ns_get_list <- function(.x, .p = NULL, ..., .default = NULL) {
  env <- ns_env(.x)
  nms <- env_names(env)
  ns_list <- env_get_list(env = env, nms = nms, default = .default)
  if (is.null(.p)) {
    return(ns_list)
  }
  nms_out <- names(ns_list)[vapply(ns_list, as_function(.p), logical(1), ...)]
  ns_list[nms_out]
}
ns_get_exports <- function(x, .p = NULL, ..., .default = NULL) {
  env <- ns_env(.x)
  nms <- getNamespaceExports(env)
  ns_list <- env_get_list(env = env, nms = nms, .efault = .default)

  if (is.null(.p)) {
    return(ns_list)
  }
  nms_out <- names(ns_list)[vapply(ns_list, as_function(.p), logical(1), ...)]
  ns_list[nms_out]
}
ns_get_private <- function(.x, .p = NULL, ..., .default = NULL) {
  env <- ns_env(.x)
  nms <- setdiff(env_names(env), getNamespaceExports(env))
  ns_list <- env_get_list(env = env, nms = nms, default = .default)

  if (is.null(.p)) {
    return(ns_list)
  }
  nms_out <- names(ns_list)[vapply(ns_list, as_function(.p), logical(1), ...)]
  ns_list[nms_out]
}

ns_get_names <- function(.x, .p = NULL, ..., .default = NULL) {
  ns_list <- ns_get_list(.x = env, .p = .p, ..., .default = .default)
  names(ns_list)
}
ns_get_exports_names <- function(.x, .p = NULL, ..., .default = NULL) {
  ns_list <- ns_get_exports(.x = env, .p = .p, ..., .default = .default)
  names(ns_list)
}
ns_get_private_names <- function(.x, .p = NULL, ..., .default = NULL) {
  ns_list <- ns_get_private(.x = env, .p = .p, ..., .default = .default)
  names(ns_list)
}
