#' `purrr` Standalone Replacement Functions
#'
#' @name purrr-standalone
NULL


#' @rdname purrr-standalone
#' @export
map <- function(.x, .f, ...) {
  .f <- as_function(.f, env = globalenv())
  lapply(.x, .f, ...)
}

#' @rdname purrr-standalone
#' @export
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

#' @rdname purrr-standalone
#' @export
map_lgl <- function(.x, .f, ...) {
  .purrr_map_mold(.x, .f, logical(1), ...)
}
#' @rdname purrr-standalone
#' @export
map_int <- function(.x, .f, ...) {
  .purrr_map_mold(.x, .f, integer(1), ...)
}

#' @rdname purrr-standalone
#' @export
map_dbl <- function(.x, .f, ...) {
  .purrr_map_mold(.x, .f, double(1), ...)
}

#' @rdname purrr-standalone
#' @export
map_chr <- function(.x, .f, ...) {
  .purrr_map_mold(.x, .f, character(1), ...)
}

.purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_function(.f, env = globalenv())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

#' @rdname purrr-standalone
#' @export
map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f, env = globalenv())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    setNames(out, names(.x))
  } else {
    setNames(out, NULL)
  }
}

#' @rdname purrr-standalone
#' @export
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}

#' @rdname purrr-standalone
#' @export
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
#' @rdname purrr-standalone
#' @export
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}

#' @rdname purrr-standalone
#' @export
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}

#' @rdname purrr-standalone
#' @export
imap <- function(.x, .f, ..., .index_only = FALSE) {
  if(.index_only){
    .y <- seq_along(.x)
  } else {
    .y <- names(.x) %||% seq_along(.x)
  }
  map2(.x, .y, .f, ...)
}

#' @rdname purrr-standalone
#' @export
pmap <- function(.l, .f, ...) {
  .f <- as_function(.f)
  args <- .purrr_args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)), args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}

.purrr_args_recycle <- function(args) {
  lengths <- vapply(args, length, integer(1))
  n <- max(lengths)
  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- lapply(args[to_recycle], function(x) {
    rep.int(
      x,
      n
    )
  })
  args
}

#' @rdname purrr-standalone
#' @export
keep <- function(.x, .f, ...) {
  .x[.purrr_probe(.x, .f, ...)]
}
#' @rdname purrr-standalone
#' @export
discard <- function(.x, .p, ...) {
  sel <- .purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}

map_at <- function(.x, .at, .f, ...) {
  if(is.numeric(.at)){
    stopifnot(all(.at %% 1 == 0) || max(.at) <= length(.x))
  }
  pos <- .at
  .x[pos] <- lapply(.x[pos], .f, ...)
  .x
}

#' @rdname purrr-standalone
#' @export
map_if <- function(.x, .p, .f, ...) {
  matches <- .purrr_probe(.x, .p)
  .x[matches] <- lapply(.x[matches], .f, ...)
  .x
}

.purrr_probe <- function(.x, .p, ...) {
  if (is.logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = globalenv())
    vapply(.x, .p, logical(1), ...)
  }
}

#' @rdname purrr-standalone
#' @export
compact <- function(.x) {
  Filter(length, .x)
}

#' @rdname purrr-standalone
#' @export
transpose <- function(.l) {
  if (!length(.l)) {
    return(.l)
  }
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- setNames(inner_names, inner_names)
    .l <- lapply(.l, function(x) {
      if (is.null(names(x))) {
        setNames(x, inner_names)
      } else {
        x
      }
    })
  }
  .l <- lapply(.l, as.list)
  lapply(fields, function(i) {
    lapply(.l, .subset2, i)
  })
}
#' @rdname purrr-standalone
#' @export
list_t <- transpose

#' @rdname purrr-standalone
#' @export
every <- function(.x, .p, ...) {
  .p <- as_function(.p, env = globalenv())
  for (i in seq_along(.x)) {
    if (!identical(.p(.x[[i]], ...), TRUE)) {
      return(FALSE)
    }
  }
  TRUE
}

#' @rdname purrr-standalone
#' @export
some <- function(.x, .p, ...) {
  .p <- as_function(.p, env = globalenv())
  for (i in seq_along(.x)) {
    if (identival(.p(.x[[i]], ...), TRUE)) {
      return(TRUE)
    }
  }
  FALSE
}

#' @rdname purrr-standalone
#' @export
negate <- function(.p) {
  .p <- as_function(.p, env = globalenv())
  function(...) !.p(...)
}

#' @rdname purrr-standalone
#' @export
reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}

#' @rdname purrr-standalone
#' @export
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}

#' @rdname purrr-standalone
#' @export
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}

#' @rdname purrr-standalone
#' @export
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

#' @rdname purrr-standalone
#' @export
detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = globalenv())
  .f <- as_function(.f, env = globalenv())
  for (i in .purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}

#' @rdname purrr-standalone
#' @export
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = globalenv())
  .f <- as_function(.f, env = globalenv())
  for (i in .purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}

.purrr_index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}
