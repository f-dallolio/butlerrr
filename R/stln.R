# nocov start

# cli ----------

#' Create unicode symbols
#'
#' The `symbol_` functions generate Unicode symbols if cli is
#' installed and Unicode is enabled. The corresponding `ansi_`
#' functions apply default ANSI colours to these symbols if possible.
#'
#' @noRd
symbol_info   <- function() if (.rlang_cli_has_cli()) cli::symbol$info else "i"
symbol_cross  <- function() if (.rlang_cli_has_cli()) cli::symbol$cross else "x"
symbol_tick   <- function() if (.rlang_cli_has_cli()) cli::symbol$tick else "v"
symbol_bullet <- function() if (.rlang_cli_has_cli()) cli::symbol$bullet else "*"
symbol_arrow  <- function() if (.rlang_cli_has_cli()) cli::symbol$arrow_right else ">"
symbol_alert  <- function() "!"

ansi_info   <- function() col_blue(symbol_info())
ansi_cross  <- function() col_red(symbol_cross())
ansi_tick   <- function() col_green(symbol_tick())
ansi_bullet <- function() col_cyan(symbol_bullet())
ansi_arrow  <- function() symbol_arrow()
ansi_alert  <- function() col_yellow(symbol_alert())


#' Apply ANSI styling
#'
#' The `col_`, `bg_`, and `style_` functions style their inputs using
#' the relevant ANSI escapes if cli is installed and ANSI colours are
#' enabled.
#'
#' @param x A string.
#'
#' @noRd
col_black              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_black(x)) else x
col_blue               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_blue(x)) else x
col_cyan               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_cyan(x)) else x
col_green              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_green(x)) else x
col_magenta            <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_magenta(x)) else x
col_red                <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_red(x)) else x
col_white              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_white(x)) else x
col_yellow             <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_yellow(x)) else x
col_grey               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_grey(x)) else x
col_silver             <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_silver(x)) else x
col_none               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_none(x)) else x

bg_black               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_black(x)) else x
bg_blue                <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_blue(x)) else x
bg_cyan                <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_cyan(x)) else x
bg_green               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_green(x)) else x
bg_magenta             <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_magenta(x)) else x
bg_red                 <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_red(x)) else x
bg_white               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_white(x)) else x
bg_yellow              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_yellow(x)) else x
bg_none                <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_none(x)) else x

style_dim              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_dim(x)) else x
style_blurred          <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_blurred(x)) else x
style_bold             <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_bold(x)) else x
style_hidden           <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_hidden(x)) else x
style_inverse          <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_inverse(x)) else x
style_italic           <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_italic(x)) else x
style_strikethrough    <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_strikethrough(x)) else x
style_underline        <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_underline(x)) else x

style_no_dim           <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_dim(x)) else x
style_no_blurred       <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_blurred(x)) else x
style_no_bold          <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_bold(x)) else x
style_no_hidden        <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_hidden(x)) else x
style_no_inverse       <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_inverse(x)) else x
style_no_italic        <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_italic(x)) else x
style_no_strikethrough <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_strikethrough(x)) else x
style_no_underline     <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_underline(x)) else x

style_reset            <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_reset(x)) else x
style_no_colour        <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_color(x)) else x
style_no_bg_colour     <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_bg_color(x)) else x

CLI_SUPPORT_HYPERLINK <- "2.2.0"
CLI_SUPPORT_HYPERLINK_PARAMS <- "3.1.1"

style_hyperlink <- function(text, url, params = NULL) {
  if (is.null(params)) {
    if (.rlang_cli_has_cli(CLI_SUPPORT_HYPERLINK)) {
      cli::style_hyperlink(text, url)
    } else {
      text
    }
  } else {
    if (.rlang_cli_has_cli(CLI_SUPPORT_HYPERLINK_PARAMS)) {
      cli::style_hyperlink(text, url, params = params)
    } else {
      text
    }
  }
}

#' Apply inline styling
#'
#' @description
#' This set of `mark_` and `format_` functions create consistent
#' inline styling, using cli if available or an ASCII fallback style
#' otherwise.
#'
#' * The `mark_` functions wrap the input with mark up tags when cli
#'   is available. For instance, `"foo"` is transformed to `{.span
#'   {\"foo\"}}`. These marked up strings must eventually be formatted
#'   using a formatter such as `format_error()` to be styled
#'   appropriately.
#'
#' * The `format_` functions are easier to work with because they
#'   format the style eagerly. However they produce slightly incorrect
#'   style in corner cases because the formatting doesn't take into
#'   account the message type. In principle, cli themes can create
#'   different stylings depending on the message type.
#'
#' @param x A string.
#'
#' @noRd
mark_emph   <- function(x) .rlang_cli_style_inline(x, "emph", "_%s_")
mark_strong <- function(x) .rlang_cli_style_inline(x, "strong", "*%s*")
mark_code   <- function(x) .rlang_cli_style_inline(x, "code", "`%s`")
mark_q      <- function(x) .rlang_cli_style_inline(x, "q", NULL)
mark_pkg    <- function(x) .rlang_cli_style_inline(x, "pkg", NULL)
mark_fn     <- function(x) .rlang_cli_style_inline(x, "fn", "`%s()`")
mark_arg    <- function(x) .rlang_cli_style_inline(x, "arg", "`%s`")
mark_kbd    <- function(x) .rlang_cli_style_inline(x, "kbd", "[%s]")
mark_key    <- function(x) .rlang_cli_style_inline(x, "key", "[%s]")
mark_file   <- function(x) .rlang_cli_style_inline(x, "file", NULL)
mark_path   <- function(x) .rlang_cli_style_inline(x, "path", NULL)
mark_email  <- function(x) .rlang_cli_style_inline(x, "email", NULL)
mark_url    <- function(x) .rlang_cli_style_inline(x, "url", "<%s>")
mark_var    <- function(x) .rlang_cli_style_inline(x, "var", "`%s`")
mark_envvar <- function(x) .rlang_cli_style_inline(x, "envvar", "`%s`")
mark_field  <- function(x) .rlang_cli_style_inline(x, "field", NULL)

mark_cls <- function(x) {
  fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
  .rlang_cli_style_inline(x, "cls", fallback)
}

format_emph   <- function(x) .rlang_cli_format_inline(x, "emph", "%s")
format_strong <- function(x) .rlang_cli_format_inline(x, "strong", "%s")
format_code   <- function(x) .rlang_cli_format_inline(x, "code", "`%s`")
format_q      <- function(x) .rlang_cli_format_inline(x, "q", NULL)
format_pkg    <- function(x) .rlang_cli_format_inline(x, "pkg", NULL)
format_fn     <- function(x) .rlang_cli_format_inline(x, "fn", "`%s()`")
format_arg    <- function(x) .rlang_cli_format_inline(x, "arg", "`%s`")
format_kbd    <- function(x) .rlang_cli_format_inline(x, "kbd", "[%s]")
format_key    <- function(x) .rlang_cli_format_inline(x, "key", "[%s]")
format_file   <- function(x) .rlang_cli_format_inline(x, "file", NULL)
format_path   <- function(x) .rlang_cli_format_inline(x, "path", NULL)
format_email  <- function(x) .rlang_cli_format_inline(x, "email", NULL)
format_url    <- function(x) .rlang_cli_format_inline(x, "url", "<%s>")
format_var    <- function(x) .rlang_cli_format_inline(x, "var", "`%s`")
format_envvar <- function(x) .rlang_cli_format_inline(x, "envvar", "`%s`")
format_field  <- function(x) .rlang_cli_format_inline(x, "field", NULL)
format_href   <- function(x, target = NULL) .rlang_cli_format_inline_link(x, target, "href", "<%s>")
format_run    <- function(x, target = NULL) .rlang_cli_format_inline_link(x, target, "run", "`%s`")

format_error_arg_highlight <- function(x, quote = TRUE) {
  if (is_true(peek_option("rlang:::trace_test_highlight"))) {
    return(paste0("<<ARG ", x, ">>"))
  }
  out <- if (quote) format_arg(x) else x
  style_bold(cli::col_br_magenta(out))
}
format_error_call_highlight <- function(x, quote = TRUE) {
  if (is_true(peek_option("rlang:::trace_test_highlight"))) {
    return(paste0("<<CALL ", x, ">>"))
  }
  out <- if (quote) format_code(x) else x
  style_bold(cli::col_br_blue(out))
}

format_cls <- function(x) {
  fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
  .rlang_cli_format_inline(x, "cls", fallback)
}

.rlang_cli_style_inline <- function(x, span, fallback = "`%s`") {
  if (.rlang_cli_has_cli()) {
    paste0("{.", span, " {\"", encodeString(x), "\"}}")
  } else if (is.null(fallback)) {
    x
  } else if (is.function(fallback)) {
    fallback(x)
  } else {
    sprintf(fallback, x)
  }
}
.rlang_cli_format_inline <- function(x, span, fallback = "`%s`") {
  if (.rlang_cli_has_cli()) {
    cli::format_inline(paste0("{.", span, " {x}}"))
  } else {
    .rlang_cli_style_inline(x, span, fallback = fallback)
  }
}

.rlang_cli_format_inline_link <- function(x, target, span, fallback = "`%s`") {
  if (.rlang_cli_has_cli()) {
    if (is_null(target)) {
      cli::format_inline(paste0("{.", span, " {x}}"))
    } else {
      cli::format_inline(paste0("{.", span, " [{x}]({target})}"))
    }
  } else {
    .rlang_cli_style_inline(x, span, fallback = fallback)
  }
}

#' Format messages
#'
#' @description
#'
#' These format functions use cli if available to format condition
#' messages. This includes structural formatting:
#'
#' - Styling as a function of the message type (error, warning,
#'   message).
#' - Bullets formatting (info, alert, ...).
#' - Indented width wrapping.
#'
#' This also applies inline formatting in combination with the
#' `style_` prefixed functions.
#'
#' The input should not contain any `"{foo}"` glue syntax. If you are
#' assembling a message from multiple pieces, use `cli_escape()` on
#' user or external inputs that might contain curly braces.
#'
#' @param x A character vector of lines. Names define bullet types.
#'
#' @noRd
format_error <- function(x) {
  .rlang_cli_format(x, cli::format_error)
}
#' @rdname format_error
#' @noRd
format_warning <- function(x) {
  .rlang_cli_format(x, cli::format_warning)
}
#' @rdname format_error
#' @noRd
format_message <- function(x) {
  .rlang_cli_format(x, cli::format_message)
}

.rlang_cli_format <- function(x, cli_format) {
  if (.rlang_cli_has_cli()) {
    out <- cli_format(x, .envir = emptyenv())
    .rlang_cli_str_restore(out, unname(x))
  } else {
    .rlang_cli_format_fallback(x)
  }
}
.rlang_cli_format_fallback <- function(x) {
  if (!length(x)) {
    return(unname(x))
  }

  nms <- names(x)

  if (is_null(nms)) {
    nms <- rep_len("", length(x))
  }

  abort <- .rlang_cli_compat("abort")

  bullets <- local({
    unicode_opt <- getOption("cli.condition_unicode_bullets")
    if (identical(unicode_opt, FALSE)) {
      old <- options(cli.unicode = FALSE)
      on.exit(options(old))
    }

    # For consistency with `cli::format_error()` and for resiliency
    # against hard-to-detect errors (see #1364), unknown names are
    # silently ignored. This also makes it easier to add new bullet
    # names in the future with forward-compatibility.
    ifelse(nms == "i", ansi_info(),
    ifelse(nms == "x", ansi_cross(),
    ifelse(nms == "v", ansi_tick(),
    ifelse(nms == "*", ansi_bullet(),
    ifelse(nms == "!", ansi_alert(),
    ifelse(nms == ">", ansi_arrow(),
    ifelse(nms == "", "",
    ifelse(nms == " ", " ",
      ""))))))))
  })

  bullets <-
    ifelse(bullets == "", "", paste0(bullets, " "))

  out <- paste0(bullets, x, collapse = "\n")
  .rlang_cli_str_restore(out, unname(x))
}

.rlang_cli_str_restore <- function(x, to) {
  out <- to

  out <- out[1]
  out[[1]] <- x

  # Restore attributes only if unclassed. It is assumed the `[` and
  # `[[` methods deal with attributes in case of classed objects.
  # Preserving attributes matters for the assertthat package for
  # instance.
  if (!is.object(to)) {
    attrib <- attributes(to)

    attrib$names <- NULL
    attrib$dim <- NULL
    attrib$dimnames <- NULL
    attrib <- c(attributes(out), attrib)

    attributes(out) <- attrib
  }

  out
}

has_ansi <- function() {
  .rlang_cli_has_cli() && cli::num_ansi_colors() > 1
}

.rlang_cli_has_cli <- local({
  cache <- new.env()

  function(version = "3.0.0") {
    out <- cache[[version]]

    if (is.null(out)) {
      out <- cache[[version]] <<-
        requireNamespace("cli", quietly = TRUE) &&
        utils::packageVersion("cli") >= version
    }

    out
  }
})

#' Escape cli and glue syntax
#'
#' This doubles all `{` and `}` characters to prevent them from being
#' interpreted as syntax for glue interpolation or cli styling.
#'
#' @param x A character vector.
#'
#' @noRd
cli_escape <- function(x) {
  if (.rlang_cli_has_cli()) {
    gsub("\\}", "}}", gsub("\\{", "{{", x))
  } else {
    x
  }
}


.rlang_cli_compat <- function(fn, try_rlang = TRUE) {
  # Compats that behave the same independently of rlang's presence
  out <- switch(
    fn,
    is_installed = return(function(pkg) requireNamespace(pkg, quietly = TRUE))
  )

  # Only use rlang if it is fully loaded (#1482)
  if (try_rlang &&
        requireNamespace("rlang", quietly = TRUE) &&
        environmentIsLocked(asNamespace("rlang"))) {
    switch(
      fn,
      is_interactive = return(rlang::is_interactive)
    )

    ns <- asNamespace("rlang")

    # Make sure rlang knows about "x" and "i" bullets.
    # Pull from namespace rather than via `utils::packageVersion()`
    # to avoid slowdown (#1657)
    if (ns[[".__NAMESPACE__."]][["spec"]][["version"]] >= "0.4.2") {
      switch(
        fn,
        abort = return(rlang::abort),
        warn = return((rlang::warn)),
        inform = return(rlang::inform)
      )
    }
  }

  # Fall back to base compats

  is_interactive_compat <- function() {
    opt <- getOption("rlang_interactive")
    if (!is.null(opt)) {
      opt
    } else {
      interactive()
    }
  }

  format_msg <- function(x) paste(x, collapse = "\n")
  switch(
    fn,
    is_interactive = return(is_interactive_compat),
    abort = return(function(msg) stop(format_msg(msg), call. = FALSE)),
    warn = return(function(msg) warning(format_msg(msg), call. = FALSE)),
    inform = return(function(msg) message(format_msg(msg)))
  )

  stop(sprintf("Internal error in rlang shims: Unknown function `%s()`.", fn))
}

.rlang_cli_unstructure <- function(x) {
  attributes(x) <- NULL
  x
}


# downstream-deps ----------

check_downstream <- function(ver,
                             ...,
                             info = NULL) {
  env <- topenv(parent.frame())
  if (!isNamespace(env)) {
    stop("`check_downstream()` must be called from a namespace.", call. = FALSE)
  }
  pkg <- unname(getNamespaceName(env))

  deps <- c(...)
  if (!is.character(deps)) {
    stop("`...` must be strings.", call. = FALSE)
  }

  deps_key <- paste0(deps, collapse = " ")
  deps <- .rlang_downstream_parse_deps(deps)

  on_package_load <- function(pkg, expr) {
    if (isNamespaceLoaded(pkg)) {
      expr
    } else {
      thunk <- function(...) expr
      setHook(packageEvent(pkg, "onLoad"), thunk)
    }
  }

  is_interactive <- .rlang_downstream_compat("is_interactive")

  if (is_interactive()) {
    cache <- .rlang_downstream_get_cache()
    cache[[pkg]][[deps_key]] <- FALSE
  }

  checked <- FALSE
  for (dep in deps) {
    on_package_load(
      dep[["pkg"]],
      .rlang_downstream_check(
        pkg,
        ver,
        deps,
        info = info,
        deps_key = deps_key
      )
    )
  }
}

.rlang_downstream_parse_deps <- function(deps) {
  str_trim <- function(x) {
    sub("^\\s+", "", sub("\\s+$", "", x))
  }
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  deps <- lapply(deps, .rlang_downstream_parse_min_requirement)
  deps
}
.rlang_downstream_parse_min_requirement <- function(dep) {
  if (length(dep) != 2) {
    stop("Parsing error during downstream check.", call. = FALSE)
  }
  is_string <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x)
  }

  parts <- strsplit(dep[[2]], " +")[[1]]
  if (length(parts) != 2) {
    stop("Parsing error during downstream check.", call. = FALSE)
  }

  op <- parts[[1]]
  ver <- parts[[2]]
  stopifnot(is_string(op), is_string(ver))

  if (op != ">=") {
    stop("Can only check `>=` requirements.", call. = FALSE)
  }

  c(pkg = dep[[1]], min = ver)
}

.rlang_downstream_check <- function(pkg,
                                    pkg_ver,
                                    deps,
                                    info,
                                    deps_key = as.character(stats::runif(1)),
                                    env = parent.frame()) {
  isFALSE <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && !x
  }

  if (isFALSE(getOption("rlib_downstream_check"))) {
    return(NULL)
  }

  # Check cache in the global environment. This cache gets saved along
  # with the session. This avoids getting repeated checks when session
  # is reloaded, e.g. when revisiting RStudio servers.
  is_interactive <- .rlang_downstream_compat("is_interactive")
  if (is_interactive()) {
    cache <- .rlang_downstream_get_cache()
    if (isTRUE(cache[[pkg]][[deps_key]])) {
      return(NULL)
    }
  }

  # Still check closure env in case the cache in the global
  # environment has been deleted
  if (isTRUE(env$checked)) {
    return(NULL)
  }

  # Don't ask again. Flip now instead of on exit to defensively
  # prevent recursion.
  if (is_interactive()) {
    cache[[pkg]][deps_key] <- list(TRUE)
  }
  env$checked <- TRUE

  pkgs <- vapply(deps, `[[`, "", "pkg")
  mins <- vapply(deps, `[[`, "", "min")

  # Don't use `requireNamespace()` to avoid loading packages
  is_on_disk <- function(pkg) nzchar(system.file(package = pkg))
  on_disk <- vapply(pkgs, is_on_disk, NA)

  pkgs <- pkgs[on_disk]
  mins <- mins[on_disk]

  vers <- lapply(pkgs, utils::packageVersion)
  ok <- as.logical(Map(`>=`, vers, mins))

  if (all(ok)) {
    return(TRUE)
  }

  pkgs <- pkgs[!ok]
  mins <- mins[!ok]

  pkgs_quoted <- paste0("`", pkgs, "` (>= ", mins, ")")
  pkgs_enum <- .rlang_downstream_collapse(pkgs_quoted, final = "and")

  n <- length(pkgs)
  if (n == 1) {
    header <- paste0("The package ", pkgs_enum, " is required")
  } else {
    header <- paste0("The packages ", pkgs_enum, " are required")
  }

  header <- sprintf("%s as of %s %s.", header, pkg, pkg_ver)

  warn <- .rlang_downstream_compat("warn")
  inform <- .rlang_downstream_compat("inform")
  is_interactive <- .rlang_downstream_compat("is_interactive")

  if (!is_interactive() || !is.null(getOption("rlang:::no_downstream_prompt"))) {
    warn(header)
    return(FALSE)
  }

  if (n == 1) {
    question <- "Would you like to update it now?"
  } else {
    question <- "Would you like to update them now?"
  }

  # Use "i" bullets by default
  if (!is.null(info) && is.null(names(info))) {
    names(info) <- rep("i", length(info))
  }

  prompt <- c(
    "!" = question,
    " " = "You will likely need to restart R if you update now.",
    info
  )
  inform(c(header, prompt))

  if (utils::menu(c("Yes", "No")) != 1) {
    inform("Set `options(rlib_downstream_check = FALSE)` to disable this prompt.")
    return(FALSE)
  }

  if (is_installed("pak")) {
    pkg_install <- get(envir = asNamespace("pak"), "pkg_install")
    pkg_install(pkgs, ask = FALSE)
  } else {
    utils::install.packages(pkgs)
  }

  TRUE
}

# Keep in sync with standalone-linked-version.R
.rlang_downstream_howto_reinstall_msg <- function(pkg) {
  os <- tolower(Sys.info()[["sysname"]])

  if (os == "windows") {
    url <- "https://github.com/jennybc/what-they-forgot/issues/62"
    c(
      i = sprintf("Please update %s to the latest version.", pkg),
      i = sprintf("Updating packages on Windows requires precautions:\n  <%s>", url)
    )
  } else {
    c(
      i = sprintf("Please update %s with `install.packages(\"%s\")` and restart R.", pkg, pkg)
    )
  }
}

.rlang_downstream_collapse <- function(x, sep = ", ", final = "or") {
  n <- length(x)

  if (n < 2) {
    return(x)
  }

  n <- length(x)
  head <- x[seq_len(n - 1)]
  last <- x[length(x)]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}

.rlang_downstream_compat <- function(fn, try_rlang = TRUE) {
  # Compats that behave the same independently of rlang's presence
  out <- switch(
    fn,
    is_installed = return(function(pkg) requireNamespace(pkg, quietly = TRUE))
  )

  # Only use rlang if it is fully loaded (#1482)
  if (try_rlang &&
        requireNamespace("rlang", quietly = TRUE) &&
        environmentIsLocked(asNamespace("rlang"))) {
    switch(
      fn,
      is_interactive = return(rlang::is_interactive)
    )

    # Make sure rlang knows about "x" and "i" bullets
    if (utils::packageVersion("rlang") >= "0.4.2") {
      switch(
        fn,
        abort = return(rlang::abort),
        warn = return((rlang::warn)),
        inform = return(rlang::inform)
      )
    }
  }

  # Fall back to base compats

  is_interactive_compat <- function() {
    opt <- getOption("rlang_interactive")
    if (!is.null(opt)) {
      opt
    } else {
      interactive()
    }
  }

  format_msg <- function(x) paste(x, collapse = "\n")
  switch(
    fn,
    is_interactive = return(is_interactive_compat),
    abort = return(function(msg) stop(format_msg(msg), call. = FALSE)),
    warn = return(function(msg) warning(format_msg(msg), call. = FALSE)),
    inform = return(function(msg) message(format_msg(msg)))
  )

  stop(sprintf("Internal error in rlang shims: Unknown function `%s()`.", fn))
}

.rlang_downstream_get_cache <- function() {
  if (!"org:r-lib" %in% search()) {
    do.call(
      attach,
      list(
        list(),
        pos = length(search()),
        name = "org:r-lib"
      )
    )
  }

  cache_env <- as.environment("org:r-lib")
  check_cache_name <- "rlang_downstream_check"
  cache <- cache_env[[check_cache_name]]

  if (is.null(cache)) {
    cache <- new.env(parent = emptyenv())
    cache_env[[check_cache_name]] <- cache
  }

  cache
}



# lazyeval ----------

warn_underscored <- function() {
  return(NULL)
  warn(paste(
    "The underscored versions are deprecated in favour of",
    "tidy evaluation idioms. Please see the documentation",
    "for `quo()` in rlang"
  ))
}
warn_text_se <- function() {
  return(NULL)
  warn("Text parsing is deprecated, please supply an expression or formula")
}

compat_lazy <- function(lazy, env = caller_env(), warn = TRUE) {
  if (warn) warn_underscored()

  if (missing(lazy)) {
    return(quo())
  }
  if (is_quosure(lazy)) {
    return(lazy)
  }
  if (is_formula(lazy)) {
    return(as_quosure(lazy, env))
  }

  out <- switch(typeof(lazy),
    symbol = ,
    language = new_quosure(lazy, env),
    character = {
      if (warn) warn_text_se()
      parse_quo(lazy[[1]], env)
    },
    logical = ,
    integer = ,
    double = {
      if (length(lazy) > 1) {
        warn("Truncating vector to length 1")
        lazy <- lazy[[1]]
      }
      new_quosure(lazy, env)
    },
    list =
      if (inherits(lazy, "lazy")) {
        lazy = new_quosure(lazy$expr, lazy$env)
      }
  )

  if (is_null(out)) {
    abort(sprintf("Can't convert a %s to a quosure", typeof(lazy)))
  } else {
    out
  }
}

compat_lazy_dots <- function(dots, env, ..., .named = FALSE) {
  if (missing(dots)) {
    dots <- list()
  }
  if (inherits(dots, c("lazy", "formula"))) {
    dots <- list(dots)
  } else {
    dots <- unclass(dots)
  }
  dots <- c(dots, list(...))

  warn <- TRUE
  for (i in seq_along(dots)) {
    dots[[i]] <- compat_lazy(dots[[i]], env, warn)
    warn <- FALSE
  }

  named <- have_name(dots)
  if (.named && any(!named)) {
    nms <- vapply(dots[!named], function(x) expr_text(get_expr(x)), character(1))
    names(dots)[!named] <- nms
  }

  names(dots) <- names2(dots)
  dots
}

compat_as_lazy <- function(quo) {
  structure(class = "lazy", list(
    expr = get_expr(quo),
    env = get_env(quo)
  ))
}
compat_as_lazy_dots <- function(...) {
  structure(class = "lazy_dots", lapply(quos(...), compat_as_lazy))
}



# lifecycle ----------

#' Signal deprecation
#'
#' @description
#' These functions provide two levels of verbosity for deprecation
#' warnings.
#'
#' * `deprecate_soft()` warns only if called directly: from the global
#'   environment (so the user can change their script) or from the
#'   package currently being tested (so the package developer can fix
#'   the package).
#'
#' * `deprecate_warn()` warns unconditionally.
#'
#' * `deprecate_stop()` fails unconditionally.
#'
#' Both functions warn only once per session by default to avoid
#' overwhelming the user with repeated warnings.
#'
#' @param msg The deprecation message.
#' @param id The id of the deprecation. A warning is issued only once
#'   for each `id`. Defaults to `msg`, but you should give a unique ID
#'   when the message is built programmatically and depends on inputs.
#' @param user_env The environment in which the deprecated function
#'   was called. The verbosity depends on whether the deprecated
#'   feature was called directly, see [rlang::env_is_user_facing()] and the
#'   documentation in the lifecycle package.
#'
#' @section Controlling verbosity:
#'
#' The verbosity of retirement warnings can be controlled with global
#' options. You'll generally want to set these options locally with
#' one of these helpers:
#'
#' * `with_lifecycle_silence()` disables all soft-deprecation and
#'   deprecation warnings.
#'
#' * `with_lifecycle_warnings()` enforces warnings for both
#'   soft-deprecated and deprecated functions. The warnings are
#'   repeated rather than signalled once per session.
#'
#' * `with_lifecycle_errors()` enforces errors for both
#'   soft-deprecated and deprecated functions.
#'
#' All the `with_` helpers have `scoped_` variants that are
#' particularly useful in testthat blocks.
#'
#' @noRd
NULL

deprecate_soft <- function(msg,
                           id = msg,
                           user_env = rlang::caller_env(2)) {
  .rlang_lifecycle_signal_stage(msg, "deprecated")

  id <- paste(id, collapse = "\n")
  verbosity <- .rlang_lifecycle_verbosity()

  invisible(switch(
    verbosity,
    quiet = NULL,
    warning = ,
    default =
      if (rlang::env_is_user_facing(user_env)) {
        always <- verbosity == "warning"
        trace <- rlang::trace_back(bottom = caller_env())
        .rlang_lifecycle_deprecate_warn0(
          msg,
          id = id,
          trace = trace,
          always = always
        )
      },
    error = deprecate_stop(msg)
  ))
}

deprecate_warn <- function(msg,
                           id = msg,
                           always = FALSE,
                           user_env = rlang::caller_env(2)) {
  .rlang_lifecycle_signal_stage(msg, "deprecated")

  id <- paste(id, collapse = "\n")
  verbosity <- .rlang_lifecycle_verbosity()

  invisible(switch(
    verbosity,
    quiet = NULL,
    warning = ,
    default = {
      direct <- rlang::env_is_user_facing(user_env)
      always <- direct && (always || verbosity == "warning")

      trace <- tryCatch(
        rlang::trace_back(bottom = rlang::caller_env()),
        error = function(...) NULL
      )

      .rlang_lifecycle_deprecate_warn0(
        msg,
        id = id,
        trace = trace,
        always = always
      )
    },
    error = deprecate_stop(msg),
  ))
}

.rlang_lifecycle_deprecate_warn0 <- function(msg,
                                             id = msg,
                                             trace = NULL,
                                             always = FALSE,
                                             call = rlang::caller_env()) {
  if (always) {
    freq <- "always"
  } else {
    freq <- "regularly"
  }

  rlang::warn(
    msg,
    class = "lifecycle_warning_deprecated",
    .frequency = freq,
    .frequency_id = id
  )
}

deprecate_stop <- function(msg) {
  msg <- cli::format_error(msg)
  .rlang_lifecycle_signal_stage(msg, "deprecated")

  stop(rlang::cnd(
    c("defunctError", "error", "condition"),
    old = NULL,
    new = NULL,
    package = NULL,
    message = msg
  ))
}

.rlang_lifecycle_signal_stage <- function(msg, stage) {
  rlang::signal(msg, "lifecycle_stage", stage = stage)
}

expect_deprecated <- function(expr, regexp = NULL, ...) {
  rlang::local_options(lifecycle_verbosity = "warning")

  if (!is.null(regexp) && rlang::is_na(regexp)) {
    rlang::abort("`regexp` can't be `NA`.")
  }

  testthat::expect_warning(
    {{ expr }},
    regexp = regexp,
    class = "lifecycle_warning_deprecated",
    ...
  )
}

local_lifecycle_silence <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "quiet"
  )
}
with_lifecycle_silence <- function(expr) {
  local_lifecycle_silence()
  expr
}

local_lifecycle_warnings <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "warning"
  )
}
with_lifecycle_warnings <- function(expr) {
  local_lifecycle_warnings()
  expr
}

local_lifecycle_errors <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "error"
  )
}
with_lifecycle_errors <- function(expr) {
  local_lifecycle_errors()
  expr
}

.rlang_lifecycle_verbosity <- function() {
  opt <- getOption("lifecycle_verbosity", "default")

  if (!rlang::is_string(opt, c("quiet", "default", "warning", "error"))) {
    options(lifecycle_verbosity = "default")
    rlang::warn(glue::glue(
      "
      The `lifecycle_verbosity` option must be set to one of:
      \"quiet\", \"default\", \"warning\", or \"error\".
      Resetting to \"default\".
      "
    ))
  }

  opt
}


# linked-version ----------

check_linked_version <- local({

  # Keep in sync with standalone-downstream-deps.R
  howto_reinstall_msg <- function(pkg) {
    os <- tolower(Sys.info()[["sysname"]])

    if (os == "windows") {
      url <- "https://github.com/jennybc/what-they-forgot/issues/62"
      c(
        i = sprintf("Please update %s to the latest version.", pkg),
        i = sprintf("Updating packages on Windows requires precautions:\n  <%s>", url)
      )
    } else {
      c(
        i = sprintf("Please update %s with `install.packages(\"%s\")` and restart R.", pkg, pkg)
      )
    }
  }

  function(pkg, with_rlang = requireNamespace("rlang", quietly = TRUE)) {
    ver <- utils::packageVersion(pkg)

    ns <- asNamespace(pkg)
    linked_ver_ptr <- ns[[paste0(pkg, "_linked_version")]]
    if (is.null(linked_ver_ptr)) {
      linked_ver <- ""
    } else {
      # Construct call to avoid NOTE when argument to `.Call()` is not
      # statically analysable
      linked_ver <- do.call(".Call", list(linked_ver_ptr))
    }

    if (nzchar(linked_ver) && ver == linked_ver) {
      return(invisible(NULL))
    }

    header <- sprintf("The %s package is not properly installed.", pkg)

    if (nzchar(linked_ver)) {
      msg <- c(x = sprintf(
        "The DLL version (%s) does not correspond to the package version (%s).",
        linked_ver,
        ver
      ))
    } else {
      # Package does not have a version pointer. This happens when DLL
      # updating fails for the first version that includes the pointer.
      msg <- c(x = "The DLL version does not correspond to the package version.")
    }

    msg <- c(msg, howto_reinstall_msg(pkg))

    if (with_rlang) {
      msg <- paste(header, rlang::format_error_bullets(msg), sep = "\n")
      rlang::abort(msg)
    } else {
      msg <- paste(c(header, msg), collapse = "\n")
      stop(msg, call. = FALSE)
    }
  }
})



# obj-type ----------

#' Return English-friendly type
#' @param x Any R object.
#' @param value Whether to describe the value of `x`. Special values
#'   like `NA` or `""` are always described.
#' @param length Whether to mention the length of vectors and lists.
#' @return A string describing the type. Starts with an indefinite
#'   article, e.g. "an integer vector".
#' @noRd
obj_type_friendly <- function(x, value = TRUE) {
  if (is_missing(x)) {
    return("absent")
  }

  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    } else {
      type <- class(x)[[1L]]
    }
    return(sprintf("a <%s> object", type))
  }

  if (!is_vector(x)) {
    return(.rlang_as_friendly_type(typeof(x)))
  }

  n_dim <- length(dim(x))

  if (!n_dim) {
    if (!is_list(x) && length(x) == 1) {
      if (is_na(x)) {
        return(switch(
          typeof(x),
          logical = "`NA`",
          integer = "an integer `NA`",
          double =
            if (is.nan(x)) {
              "`NaN`"
            } else {
              "a numeric `NA`"
            },
          complex = "a complex `NA`",
          character = "a character `NA`",
          .rlang_stop_unexpected_typeof(x)
        ))
      }

      show_infinites <- function(x) {
        if (x > 0) {
          "`Inf`"
        } else {
          "`-Inf`"
        }
      }
      str_encode <- function(x, width = 30, ...) {
        if (nchar(x) > width) {
          x <- substr(x, 1, width - 3)
          x <- paste0(x, "...")
        }
        encodeString(x, ...)
      }

      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }

        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x)) "the complex number" else "the number"
          return(paste(what, number))
        }

        return(switch(
          typeof(x),
          logical = if (x) "`TRUE`" else "`FALSE`",
          character = {
            what <- if (nzchar(x)) "the string" else "the empty string"
            paste(what, str_encode(x, quote = "\""))
          },
          raw = paste("the raw value", as.character(x)),
          .rlang_stop_unexpected_typeof(x)
        ))
      }

      return(switch(
        typeof(x),
        logical = "a logical value",
        integer = "an integer",
        double = if (is.infinite(x)) show_infinites(x) else "a number",
        complex = "a complex number",
        character = if (nzchar(x)) "a string" else "\"\"",
        raw = "a raw value",
        .rlang_stop_unexpected_typeof(x)
      ))
    }

    if (length(x) == 0) {
      return(switch(
        typeof(x),
        logical = "an empty logical vector",
        integer = "an empty integer vector",
        double = "an empty numeric vector",
        complex = "an empty complex vector",
        character = "an empty character vector",
        raw = "an empty raw vector",
        list = "an empty list",
        .rlang_stop_unexpected_typeof(x)
      ))
    }
  }

  vec_type_friendly(x)
}

vec_type_friendly <- function(x, length = FALSE) {
  if (!is_vector(x)) {
    abort("`x` must be a vector.")
  }
  type <- typeof(x)
  n_dim <- length(dim(x))

  add_length <- function(type) {
    if (length && !n_dim) {
      paste0(type, sprintf(" of length %s", length(x)))
    } else {
      type
    }
  }

  if (type == "list") {
    if (n_dim < 2) {
      return(add_length("a list"))
    } else if (is.data.frame(x)) {
      return("a data frame")
    } else if (n_dim == 2) {
      return("a list matrix")
    } else {
      return("a list array")
    }
  }

  type <- switch(
    type,
    logical = "a logical %s",
    integer = "an integer %s",
    numeric = ,
    double = "a double %s",
    complex = "a complex %s",
    character = "a character %s",
    raw = "a raw %s",
    type = paste0("a ", type, " %s")
  )

  if (n_dim < 2) {
    kind <- "vector"
  } else if (n_dim == 2) {
    kind <- "matrix"
  } else {
    kind <- "array"
  }
  out <- sprintf(type, kind)

  if (n_dim >= 2) {
    out
  } else {
    add_length(out)
  }
}

.rlang_as_friendly_type <- function(type) {
  switch(
    type,

    list = "a list",

    NULL = "`NULL`",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",

    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal `any` object",
    bytecode = "an internal bytecode object",

    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",

    type
  )
}

.rlang_stop_unexpected_typeof <- function(x, call = caller_env()) {
  abort(
    sprintf("Unexpected type <%s>.", typeof(x)),
    call = call
  )
}

#' Return OO type
#' @param x Any R object.
#' @return One of `"bare"` (for non-OO objects), `"S3"`, `"S4"`,
#'   `"R6"`, or `"S7"`.
#' @noRd
obj_type_oo <- function(x) {
  if (!is.object(x)) {
    return("bare")
  }

  class <- inherits(x, c("R6", "S7_object"), which = TRUE)

  if (class[[1]]) {
    "R6"
  } else if (class[[2]]) {
    "S7"
  } else if (isS4(x)) {
    "S4"
  } else {
    "S3"
  }
}

#' @param x The object type which does not conform to `what`. Its
#'   `obj_type_friendly()` is taken and mentioned in the error message.
#' @param what The friendly expected type as a string. Can be a
#'   character vector of expected types, in which case the error
#'   message mentions all of them in an "or" enumeration.
#' @param show_value Passed to `value` argument of `obj_type_friendly()`.
#' @param ... Arguments passed to [abort()].
#' @inheritParams args_error_context
#' @noRd
stop_input_type <- function(x,
                            what,
                            ...,
                            allow_na = FALSE,
                            allow_null = FALSE,
                            show_value = TRUE,
                            arg = caller_arg(x),
                            call = caller_env()) {
  # From standalone-cli.R
  cli <- env_get_list(
    nms = c("format_arg", "format_code"),
    last = topenv(),
    default = function(x) sprintf("`%s`", x),
    inherit = TRUE
  )

  if (allow_na) {
    what <- c(what, cli$format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, cli$format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }
  if (inherits(arg, "AsIs")) {
    format_arg <- identity
  } else {
    format_arg <- cli$format_arg
  }

  message <- sprintf(
    "%s must be %s, not %s.",
    format_arg(arg),
    what,
    obj_type_friendly(x, value = show_value)
  )

  abort(message, ..., call = call, arg = arg)
}

oxford_comma <- function(chr, sep = ", ", final = "or") {
  n <- length(chr)

  if (n < 2) {
    return(chr)
  }

  head <- chr[seq_len(n - 1)]
  last <- chr[n]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}


# purrr ----------

map <- function(.x, .f, ...) {
  .f <- as_function(.f, env = global_env())
  lapply(.x, .f, ...)
}
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

map_lgl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, character(1), ...)
}
.rlang_purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_function(.f, env = global_env())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f, env = global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}

pmap <- function(.l, .f, ...) {
  .f <- as.function(.f)
  args <- .rlang_purrr_args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}
.rlang_purrr_args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}

keep <- function(.x, .f, ...) {
  .x[.rlang_purrr_probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- .rlang_purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
map_if <- function(.x, .p, .f, ...) {
  matches <- .rlang_purrr_probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}
.rlang_purrr_probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = global_env())
    map_lgl(.x, .p, ...)
  }
}

compact <- function(.x) {
  Filter(length, .x)
}

transpose <- function(.l) {
  if (!length(.l)) {
    return(.l)
  }

  inner_names <- names(.l[[1]])

  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
    .l <- map(.l, function(x) {
      if (is.null(names(x))) {
        set_names(x, inner_names)
      } else {
        x
      }
    })
  }

  # This way missing fields are subsetted as `NULL` instead of causing
  # an error
  .l <- map(.l, as.list)

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

every <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())

  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())

  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  .p <- as_function(.p, env = global_env())
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())

  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())

  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
.rlang_purrr_index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}

list_c <- function(x) {
  inject(c(!!!x))
}


# rlang ----------

# These versions of `abort()`, `warn()` and `inform()` are only
# guaranteed to support "i" and "x" bullets. Other kinds of bullets
# might fail if rlang is not recent enough.


.rlang_compat <- function(fn, try_rlang = TRUE) {
  # Compats that behave the same independently of rlang's presence
  out <- switch(
    fn,
    is_installed = return(function(pkg) requireNamespace(pkg, quietly = TRUE))
  )

  # Only use rlang if it is fully loaded (#1482)
  if (try_rlang &&
        requireNamespace("rlang", quietly = TRUE) &&
        environmentIsLocked(asNamespace("rlang"))) {
    switch(
      fn,
      is_interactive = return(rlang::is_interactive)
    )

    # Make sure rlang knows about "x" and "i" bullets
    if (utils::packageVersion("rlang") >= "0.4.2") {
      switch(
        fn,
        abort = return(rlang::abort),
        warn = return((rlang::warn)),
        inform = return(rlang::inform)
      )
    }
  }

  # Fall back to base compats

  is_interactive_compat <- function() {
    opt <- getOption("rlang_interactive")
    if (!is.null(opt)) {
      opt
    } else {
      interactive()
    }
  }

  format_msg <- function(x) paste(x, collapse = "\n")
  switch(
    fn,
    is_interactive = return(is_interactive_compat),
    abort = return(function(msg) stop(format_msg(msg), call. = FALSE)),
    warn = return(function(msg) warning(format_msg(msg), call. = FALSE)),
    inform = return(function(msg) message(format_msg(msg)))
  )

  stop(sprintf("Internal error in rlang shims: Unknown function `%s()`.", fn))
}



# s3-register ----------

#' Register a method for a suggested dependency
#'
#' Generally, the recommended way to register an S3 method is to use the
#' `S3Method()` namespace directive (often generated automatically by the
#' `@export` roxygen2 tag). However, this technique requires that the generic
#' be in an imported package, and sometimes you want to suggest a package,
#' and only provide a method when that package is loaded. `s3_register()`
#' can be called from your package's `.onLoad()` to dynamically register
#' a method only if the generic's package is loaded.
#'
#' For R 3.5.0 and later, `s3_register()` is also useful when demonstrating
#' class creation in a vignette, since method lookup no longer always involves
#' the lexical scope. For R 3.6.0 and later, you can achieve a similar effect
#' by using "delayed method registration", i.e. placing the following in your
#' `NAMESPACE` file:
#'
#' ```
#' if (getRversion() >= "3.6.0") {
#'   S3method(package::generic, class)
#' }
#' ```
#'
#' @section Usage in other packages:
#' To avoid taking a dependency on rlang, you copy the source of
#' [`s3_register()`](https://github.com/r-lib/rlang/blob/main/R/standalone-s3-register.R)
#' into your own package or with
#' `usethis::use_standalone("r-lib/rlang", "s3-register")`. It is licensed under
#' the permissive [unlicense](https://choosealicense.com/licenses/unlicense/) to
#' make it crystal clear that we're happy for you to do this. There's no need to
#' include the license or even credit us when using this function.
#'
#' @param generic Name of the generic in the form `"pkg::generic"`.
#' @param class Name of the class
#' @param method Optionally, the implementation of the method. By default,
#'   this will be found by looking for a function called `generic.class`
#'   in the package environment.
#' @examples
#' # A typical use case is to dynamically register tibble/pillar methods
#' # for your class. That way you avoid creating a hard dependency on packages
#' # that are not essential, while still providing finer control over
#' # printing when they are used.
#'
#' .onLoad <- function(...) {
#'   s3_register("pillar::pillar_shaft", "vctrs_vctr")
#'   s3_register("tibble::type_sum", "vctrs_vctr")
#' }
#' @keywords internal
#' @noRd
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  register <- function(...) {
    envir <- asNamespace(package)

    # Refresh the method each time, it might have been updated by
    # `devtools::load_all()`
    method_fn <- get_method(method)
    stopifnot(is.function(method_fn))


    # Only register if generic can be accessed
    if (exists(generic, envir)) {
      registerS3method(generic, class, method_fn, envir = envir)
    } else if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      warn <- .rlang_s3_register_compat("warn")

      warn(c(
        sprintf(
          "Can't find generic `%s` in package %s to register S3 method.",
          generic,
          package
        ),
        "i" = "This message is only shown to developers using devtools.",
        "i" = sprintf("Do you need to update %s to the latest version?", package)
      ))
    }
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(package, "onLoad"), function(...) {
    register()
  })

  # For compatibility with R < 4.1.0 where base isn't locked
  is_sealed <- function(pkg) {
    identical(pkg, "base") || environmentIsLocked(asNamespace(pkg))
  }

  # Avoid registration failures during loading (pkgload or regular).
  # Check that environment is locked because the registering package
  # might be a dependency of the package that exports the generic. In
  # that case, the exports (and the generic) might not be populated
  # yet (#1225).
  if (isNamespaceLoaded(package) && is_sealed(package)) {
    register()
  }

  invisible()
}

.rlang_s3_register_compat <- function(fn, try_rlang = TRUE) {
  # Compats that behave the same independently of rlang's presence
  out <- switch(
    fn,
    is_installed = return(function(pkg) requireNamespace(pkg, quietly = TRUE))
  )

  # Only use rlang if it is fully loaded (#1482)
  if (try_rlang &&
        requireNamespace("rlang", quietly = TRUE) &&
        environmentIsLocked(asNamespace("rlang"))) {
    switch(
      fn,
      is_interactive = return(rlang::is_interactive)
    )

    # Make sure rlang knows about "x" and "i" bullets
    if (utils::packageVersion("rlang") >= "0.4.2") {
      switch(
        fn,
        abort = return(rlang::abort),
        warn = return((rlang::warn)),
        inform = return(rlang::inform)
      )
    }
  }

  # Fall back to base compats

  is_interactive_compat <- function() {
    opt <- getOption("rlang_interactive")
    if (!is.null(opt)) {
      opt
    } else {
      interactive()
    }
  }

  format_msg <- function(x) paste(x, collapse = "\n")
  switch(
    fn,
    is_interactive = return(is_interactive_compat),
    abort = return(function(msg) stop(format_msg(msg), call. = FALSE)),
    warn = return(function(msg) warning(format_msg(msg), call. = FALSE)),
    inform = return(function(msg) message(format_msg(msg)))
  )

  stop(sprintf("Internal error in rlang shims: Unknown function `%s()`.", fn))
}


# sizes ----------

format_bytes <- local({

  pretty_bytes <- function(bytes, style = c("default", "nopad", "6")) {

    style <- switch(
      match.arg(style),
      "default" = pretty_bytes_default,
      "nopad" = pretty_bytes_nopad,
      "6" = pretty_bytes_6
    )

    style(bytes)
  }

  compute_bytes <- function(bytes, smallest_unit = "B") {
    units0 <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")

    stopifnot(
      is.numeric(bytes),
      is.character(smallest_unit),
      length(smallest_unit) == 1,
      !is.na(smallest_unit),
      smallest_unit %in% units0
    )

    limits <- c(1000, 999950 * 1000 ^ (seq_len(length(units0) - 2) - 1))
    low <- match(smallest_unit, units0)
    units <- units0[low:length(units0)]
    limits <- limits[low:length(limits)]

    neg <- bytes < 0 & !is.na(bytes)
    bytes <- abs(bytes)

    mat <- matrix(
      rep(bytes, each = length(limits)),
      nrow = length(limits),
      ncol = length(bytes)
    )
    mat2 <- matrix(mat < limits, nrow  = length(limits), ncol = length(bytes))
    exponent <- length(limits) - colSums(mat2) + low - 1L
    res <- bytes / 1000 ^ exponent
    unit <- units[exponent - low + 2L]

    ## Zero bytes
    res[bytes == 0] <- 0
    unit[bytes == 0] <- units[1]

    ## NA and NaN bytes
    res[is.na(bytes)] <- NA_real_
    res[is.nan(bytes)] <- NaN
    unit[is.na(bytes)] <- units0[low]     # Includes NaN as well

    data.frame(
      stringsAsFactors = FALSE,
      amount = res,
      unit = unit,
      negative = neg
    )
  }

  pretty_bytes_default <- function(bytes) {
    szs <- compute_bytes(bytes)
    amt <- szs$amount

    ## String. For fractions we always show two fraction digits
    res <- character(length(amt))
    int <- is.na(amt) | amt == as.integer(amt)
    res[int] <- format(
      ifelse(szs$negative[int], -1, 1) * amt[int],
      scientific = FALSE
    )
    res[!int] <- sprintf("%.2f", ifelse(szs$negative[!int], -1, 1) * amt[!int])

    format(paste(res, szs$unit), justify = "right")
  }

  pretty_bytes_nopad <- function(bytes) {
    sub("^\\s+", "", pretty_bytes_default(bytes))
  }

  pretty_bytes_6 <- function(bytes) {
    szs <- compute_bytes(bytes, smallest_unit = "kB")
    amt <- szs$amount

    na   <- is.na(amt)
    nan  <- is.nan(amt)
    neg  <- !na & !nan & szs$negative
    l10  <- !na & !nan & !neg & amt < 10
    l100 <- !na & !nan & !neg & amt >= 10 & amt < 100
    b100 <- !na & !nan & !neg & amt >= 100

    szs$unit[neg] <- "kB"

    famt <- character(length(amt))
    famt[na] <- " NA"
    famt[nan] <- "NaN"
    famt[neg] <- "< 0"
    famt[l10] <- sprintf("%.1f", amt[l10])
    famt[l100] <- sprintf(" %.0f", amt[l100])
    famt[b100] <- sprintf("%.0f", amt[b100])

    paste0(famt, " ", szs$unit)
  }

  structure(
    list(
      .internal     = environment(),
      pretty_bytes  = pretty_bytes,
      compute_bytes = compute_bytes
    ),
    class = c("standalone_bytes", "standalone")
  )
})


# types-check ----------

## 1. scalars ----------

.standalone_types_check_dot_call <- .Call

check_bool <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x) && .standalone_types_check_dot_call(ffi_standalone_is_bool_1.0.7, x, allow_na, allow_null)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    c("`TRUE`", "`FALSE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_string <- function(x,
                         ...,
                         allow_empty = TRUE,
                         allow_na = FALSE,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!missing(x)) {
    is_string <- .rlang_check_is_string(
      x,
      allow_empty = allow_empty,
      allow_na = allow_na,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a single string",
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

.rlang_check_is_string <- function(x,
                                   allow_empty,
                                   allow_na,
                                   allow_null) {
  if (is_string(x)) {
    if (allow_empty || !is_string(x, "")) {
      return(TRUE)
    }
  }

  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  if (allow_na && (identical(x, NA) || identical(x, na_chr))) {
    return(TRUE)
  }

  FALSE
}

check_name <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x)) {
    is_string <- .rlang_check_is_string(
      x,
      allow_empty = FALSE,
      allow_na = FALSE,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a valid name",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

IS_NUMBER_true <- 0
IS_NUMBER_false <- 1
IS_NUMBER_oob <- 2

check_number_decimal <- function(x,
                                 ...,
                                 min = NULL,
                                 max = NULL,
                                 allow_infinite = TRUE,
                                 allow_na = FALSE,
                                 allow_null = FALSE,
                                 arg = caller_arg(x),
                                 call = caller_env()) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (0 == (exit_code <- .standalone_types_check_dot_call(
    ffi_standalone_check_number_1.0.7,
    x,
    allow_decimal = TRUE,
    min,
    max,
    allow_infinite,
    allow_na,
    allow_null
  ))) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = TRUE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_number_whole <- function(x,
                               ...,
                               min = NULL,
                               max = NULL,
                               allow_infinite = FALSE,
                               allow_na = FALSE,
                               allow_null = FALSE,
                               arg = caller_arg(x),
                               call = caller_env()) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (0 == (exit_code <- .standalone_types_check_dot_call(
    ffi_standalone_check_number_1.0.7,
    x,
    allow_decimal = FALSE,
    min,
    max,
    allow_infinite,
    allow_na,
    allow_null
  ))) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = FALSE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

.stop_not_number <- function(x,
                             ...,
                             exit_code,
                             allow_decimal,
                             min,
                             max,
                             allow_na,
                             allow_null,
                             arg,
                             call) {
  if (allow_decimal) {
    what <- "a number"
  } else {
    what <- "a whole number"
  }

  if (exit_code == IS_NUMBER_oob) {
    min <- min %||% -Inf
    max <- max %||% Inf

    if (min > -Inf && max < Inf) {
      what <- sprintf("%s between %s and %s", what, min, max)
    } else if (x < min) {
      what <- sprintf("%s larger than or equal to %s", what, min)
    } else if (x > max) {
      what <- sprintf("%s smaller than or equal to %s", what, max)
    } else {
      abort("Unexpected state in OOB check", .internal = TRUE)
    }
  }

  stop_input_type(
    x,
    what,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_symbol <- function(x,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!missing(x)) {
    if (is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a symbol",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_arg <- function(x,
                      ...,
                      allow_null = FALSE,
                      arg = caller_arg(x),
                      call = caller_env()) {
  if (!missing(x)) {
    if (is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an argument name",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_call <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x)) {
    if (is_call(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused call",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_environment <- function(x,
                              ...,
                              allow_null = FALSE,
                              arg = caller_arg(x),
                              call = caller_env()) {
  if (!missing(x)) {
    if (is_environment(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an environment",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_function <- function(x,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!missing(x)) {
    if (is_function(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a function",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_closure <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!missing(x)) {
    if (is_closure(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an R function",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_formula <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!missing(x)) {
    if (is_formula(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a formula",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


## 2. vectors ----------

# TODO: Figure out what to do with logical `NA` and `allow_na = TRUE`

check_character <- function(x,
                            ...,
                            allow_na = TRUE,
                            allow_null = FALSE,
                            arg = caller_arg(x),
                            call = caller_env()) {

  if (!missing(x)) {
    if (is_character(x)) {
      if (!allow_na && any(is.na(x))) {
        abort(
          sprintf("`%s` can't contain NA values.", arg),
          arg = arg,
          call = call
        )
      }

      return(invisible(NULL))
    }

    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a character vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_logical <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!missing(x)) {
    if (is_logical(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a logical vector",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_data_frame <- function(x,
                             ...,
                             allow_null = FALSE,
                             arg = caller_arg(x),
                             call = caller_env()) {
  if (!missing(x)) {
    if (is.data.frame(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a data frame",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


# vctrs ----------

## 1. construction ----------

# Constructs data frames inheriting from `"tbl"`. This allows the
# pillar package to take over printing as soon as it is loaded.
# The data frame otherwise behaves like a base data frame.
data_frame <- function(...) {
  new_data_frame(df_list(...), .class = "tbl")
}

new_data_frame <- function(.x = list(),
                           ...,
                           .size = NULL,
                           .class = NULL) {
  n_cols <- length(.x)
  if (n_cols != 0 && is.null(names(.x))) {
    stop("Columns must be named.", call. = FALSE)
  }

  if (is.null(.size)) {
    if (n_cols == 0) {
      .size <- 0
    } else {
      .size <- vec_size(.x[[1]])
    }
  }

  structure(
    .x,
    class = c(.class, "data.frame"),
    row.names = .set_row_names(.size),
    ...
  )
}

df_list <- function(..., .size = NULL) {
  vec_recycle_common(list(...), size = .size)
}


## 2. binding ----------

vec_rbind <- function(...) {
  xs <- vec_cast_common(list(...))
  do.call(base::rbind, xs)
}

vec_cbind <- function(...) {
  xs <- list(...)

  ptype <- vec_ptype_common(lapply(xs, `[`, 0))
  class <- setdiff(class(ptype), "data.frame")

  xs <- vec_recycle_common(xs)
  out <- do.call(base::cbind, xs)
  new_data_frame(out, .class = class)
}


## 3. slicing ----------

vec_size <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else {
    length(x)
  }
}

vec_rep <- function(x, times) {
  i <- rep.int(seq_len(vec_size(x)), times)
  vec_slice(x, i)
}

vec_recycle_common <- function(xs, size = NULL) {
  sizes <- vapply(xs, vec_size, integer(1))

  n <- unique(sizes)

  if (length(n) == 1 && is.null(size)) {
    return(xs)
  }
  n <- setdiff(n, 1L)

  ns <- length(n)

  if (ns == 0) {
    if (is.null(size)) {
      return(xs)
    }
  } else if (ns == 1) {
    if (is.null(size)) {
      size <- n
    } else if (n != size) {
      stop("Inputs can't be recycled to `size`.", call. = FALSE)
    }
  } else {
    stop("Inputs can't be recycled to a common size.", call. = FALSE)
  }

  to_recycle <- sizes == 1L
  xs[to_recycle] <- lapply(xs[to_recycle], vec_rep, size)

  xs
}

vec_slice <- function(x, i) {
  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(is.numeric(i) || is.character(i))

  if (is.null(x)) {
    return(NULL)
  }

  if (is.data.frame(x)) {
    # We need to be a bit careful to be generic. First empty all
    # columns and expand the df to final size.
    out <- x[i, 0, drop = FALSE]

    # Then fill in with sliced columns
    out[seq_along(x)] <- lapply(x, vec_slice, i)

    # Reset automatic row names to work around `[` weirdness
    if (is.numeric(attr(x, "row.names"))) {
      row_names <- .set_row_names(nrow(out))
    } else {
      row_names <- attr(out, "row.names")
    }

    # Restore attributes
    mtd <- .rlang_vctrs_s3_method("[", class(x))
    if (is_null(mtd) || identical(environment(mtd), asNamespace("base"))) {
      attrib <- attributes(x)
      attrib$row.names <- row_names
      attributes(out) <- attrib
    }

    return(out)
  }

  d <- vec_dims(x)
  if (d == 1) {
    if (is.object(x)) {
      out <- x[i]
    } else {
      out <- x[i, drop = FALSE]
    }
  } else if (d == 2) {
    out <- x[i, , drop = FALSE]
  } else {
    j <- rep(list(quote(expr = )), d - 1)
    out <- eval(as.call(list(quote(`[`), quote(x), quote(i), j, drop = FALSE)))
  }

  mtd <- .rlang_vctrs_s3_method("[", class(x))
  if (is_null(mtd) || identical(environment(mtd), asNamespace("base"))) {
    attrib <- attributes(x)
    attrib$names <- attr(out, "names")
    attrib$dim <- attr(out, "dim")
    attrib$dim.names <- attr(out, "dim.names")
    attributes(out) <- attrib
  }

  out
}
vec_dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    1L
  } else {
    length(d)
  }
}

vec_as_location <- function(i, n, names = NULL) {
  out <- seq_len(n)
  names(out) <- names

  # Special-case recycling to size 0
  if (is_logical(i, n = 1) && !length(out)) {
    return(out)
  }

  unname(out[i])
}

vec_init <- function(x, n = 1L) {
  vec_slice(x, rep_len(NA_integer_, n))
}

vec_assign <- function(x, i, value) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(
    is.numeric(i) || is.character(i)
  )

  value <- vec_recycle(value, vec_size(i))
  value <- vec_cast(value, to = x)

  d <- vec_dims(x)

  if (d == 1) {
    x[i] <- value
  } else if (d == 2) {
    x[i, ] <- value
  } else {
    stop("Can't slice-assign arrays.", call. = FALSE)
  }

  x
}

vec_recycle <- function(x, size) {
  if (is.null(x) || is.null(size)) {
    return(NULL)
  }

  n_x <- vec_size(x)

  if (n_x == size) {
    x
  } else if (size == 0L) {
    vec_slice(x, 0L)
  } else if (n_x == 1L) {
    vec_slice(x, rep(1L, size))
  } else {
    stop("Incompatible lengths: ", n_x, ", ", size, call. = FALSE)
  }
}


## 4. coercion ----------

vec_cast_common <- function(xs, to = NULL) {
  ptype <- vec_ptype_common(xs, ptype = to)
  lapply(xs, vec_cast, to = ptype)
}

vec_cast <- function(x, to) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.null(to)) {
    return(x)
  }

  if (vec_is_unspecified(x)) {
    return(vec_init(to, vec_size(x)))
  }

  stop_incompatible_cast <- function(x, to) {
    stop(
      sprintf("Can't convert <%s> to <%s>.",
        .rlang_vctrs_typeof(x),
        .rlang_vctrs_typeof(to)
      ),
      call. = FALSE
    )
  }

  lgl_cast <- function(x, to) {
    lgl_cast_from_num <- function(x) {
      if (any(!x %in% c(0L, 1L))) {
        stop_incompatible_cast(x, to)
      }
      as.logical(x)
    }

    switch(
      .rlang_vctrs_typeof(x),
      logical = x,
      integer = ,
      double = lgl_cast_from_num(x),
      stop_incompatible_cast(x, to)
    )
  }

  int_cast <- function(x, to) {
    int_cast_from_dbl <- function(x) {
      out <- suppressWarnings(as.integer(x))
      if (any((out != x) | xor(is.na(x), is.na(out)))) {
        stop_incompatible_cast(x, to)
      } else {
        out
      }
    }

    switch(
      .rlang_vctrs_typeof(x),
      logical = as.integer(x),
      integer = x,
      double = int_cast_from_dbl(x),
      stop_incompatible_cast(x, to)
    )
  }

  dbl_cast <- function(x, to) {
    switch(
      .rlang_vctrs_typeof(x),
      logical = ,
      integer = as.double(x),
      double = x,
      stop_incompatible_cast(x, to)
    )
  }

  chr_cast <- function(x, to) {
    switch(
      .rlang_vctrs_typeof(x),
      character = x,
      stop_incompatible_cast(x, to)
    )
  }

  list_cast <- function(x, to) {
    switch(
      .rlang_vctrs_typeof(x),
      list = x,
      stop_incompatible_cast(x, to)
    )
  }

  df_cast <- function(x, to) {
    # Check for extra columns
    if (length(setdiff(names(x), names(to))) > 0 ) {
      stop("Can't convert data frame because of missing columns.", call. = FALSE)
    }

    # Avoid expensive [.data.frame method
    out <- as.list(x)

    # Coerce common columns
    common <- intersect(names(x), names(to))
    out[common] <- Map(vec_cast, out[common], to[common])

    # Add new columns
    from_type <- setdiff(names(to), names(x))
    out[from_type] <- lapply(to[from_type], vec_init, n = vec_size(x))

    # Ensure columns are ordered according to `to`
    out <- out[names(to)]

    new_data_frame(out)
  }

  rlib_df_cast <- function(x, to) {
    new_data_frame(df_cast(x, to), .class = "tbl")
  }
  tib_cast <- function(x, to) {
    new_data_frame(df_cast(x, to), .class = c("tbl_df", "tbl"))
  }

  switch(
    .rlang_vctrs_typeof(to),
    logical = lgl_cast(x, to),
    integer = int_cast(x, to),
    double = dbl_cast(x, to),
    character = chr_cast(x, to),
    list = list_cast(x, to),

    base_data_frame = df_cast(x, to),
    rlib_data_frame = rlib_df_cast(x, to),
    tibble = tib_cast(x, to),

    stop_incompatible_cast(x, to)
  )
}

vec_ptype_common <- function(xs, ptype = NULL) {
  if (!is.null(ptype)) {
    return(vec_ptype(ptype))
  }

  xs <- Filter(function(x) !is.null(x), xs)

  if (length(xs) == 0) {
    return(NULL)
  }

  if (length(xs) == 1) {
    out <- vec_ptype(xs[[1]])
  } else {
    xs <- map(xs, vec_ptype)
    out <- Reduce(vec_ptype2, xs)
  }

  vec_ptype_finalise(out)
}

vec_ptype_finalise <- function(x) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, vec_ptype_finalise)
    return(x)
  }

  if (inherits(x, "rlang_unspecified")) {
    logical()
  } else {
    x
  }
}

vec_ptype <- function(x) {
  if (vec_is_unspecified(x)) {
    return(.rlang_vctrs_unspecified())
  }

  if (is.data.frame(x)) {
    out <- new_data_frame(lapply(x, vec_ptype))

    attrib <- attributes(x)
    attrib$row.names <- attr(out, "row.names")
    attributes(out) <- attrib

    return(out)
  }

  vec_slice(x, 0)
}

vec_ptype2 <- function(x, y) {
  stop_incompatible_type <- function(x, y) {
    stop(
      sprintf("Can't combine types <%s> and <%s>.",
        .rlang_vctrs_typeof(x),
        .rlang_vctrs_typeof(y)),
      call. = FALSE
    )
  }

  x_type <- .rlang_vctrs_typeof(x)
  y_type <- .rlang_vctrs_typeof(y)

  if (x_type == "unspecified" && y_type == "unspecified") {
    return(.rlang_vctrs_unspecified())
  }
  if (x_type == "unspecified") {
    return(y)
  }
  if (y_type == "unspecified") {
    return(x)
  }

  df_ptype2 <- function(x, y) {
    set_partition <- function(x, y) {
      list(
        both = intersect(x, y),
        only_x = setdiff(x, y),
        only_y = setdiff(y, x)
      )
    }

    # Avoid expensive [.data.frame
    x <- as.list(vec_slice(x, 0))
    y <- as.list(vec_slice(y, 0))

    # Find column types
    names <- set_partition(names(x), names(y))
    if (length(names$both) > 0) {
      common_types <- Map(vec_ptype2, x[names$both], y[names$both])
    } else {
      common_types <- list()
    }
    only_x_types <- x[names$only_x]
    only_y_types <- y[names$only_y]

    # Combine and construct
    out <- c(common_types, only_x_types, only_y_types)
    out <- out[c(names(x), names$only_y)]
    new_data_frame(out)
  }

  rlib_df_ptype2 <- function(x, y) {
    new_data_frame(df_ptype2(x, y), .class = "tbl")
  }
  tib_ptype2 <- function(x, y) {
    new_data_frame(df_ptype2(x, y), .class = c("tbl_df", "tbl"))
  }

  ptype <- switch(
    x_type,

    logical = switch(
      y_type,
      logical = x,
      integer = y,
      double = y,
      stop_incompatible_type(x, y)
    ),

    integer = switch(
      .rlang_vctrs_typeof(y),
      logical = x,
      integer = x,
      double = y,
      stop_incompatible_type(x, y)
    ),

    double = switch(
      .rlang_vctrs_typeof(y),
      logical = x,
      integer = x,
      double = x,
      stop_incompatible_type(x, y)
    ),

    character = switch(
      .rlang_vctrs_typeof(y),
      character = x,
      stop_incompatible_type(x, y)
    ),

    list = switch(
      .rlang_vctrs_typeof(y),
      list = x,
      stop_incompatible_type(x, y)
    ),

    base_data_frame = switch(
      .rlang_vctrs_typeof(y),
      base_data_frame = ,
      s3_data_frame = df_ptype2(x, y),
      rlib_data_frame = rlib_df_ptype2(x, y),
      tibble = tib_ptype2(x, y),
      stop_incompatible_type(x, y)
    ),

    rlib_data_frame = switch(
      .rlang_vctrs_typeof(y),
      base_data_frame = ,
      rlib_data_frame = ,
      s3_data_frame = rlib_df_ptype2(x, y),
      tibble = tib_ptype2(x, y),
      stop_incompatible_type(x, y)
    ),

    tibble = switch(
      .rlang_vctrs_typeof(y),
      base_data_frame = ,
      rlib_data_frame = ,
      tibble = ,
      s3_data_frame = tib_ptype2(x, y),
      stop_incompatible_type(x, y)
    ),

    stop_incompatible_type(x, y)
  )

  vec_slice(ptype, 0)
}

.rlang_vctrs_typeof <- function(x) {
  if (is.object(x)) {
    class <- class(x)

    if (identical(class, "rlang_unspecified")) {
      return("unspecified")
    }
    if (identical(class, "data.frame")) {
      return("base_data_frame")
    }
    if (identical(class, c("tbl", "data.frame"))) {
      return("rlib_data_frame")
    }
    if (identical(class, c("tbl_df", "tbl", "data.frame"))) {
      return("tibble")
    }
    if (inherits(x, "data.frame")) {
      return("s3_data_frame")
    }

    class <- paste0(class, collapse = "/")
    stop(sprintf("Unimplemented class <%s>.", class), call. = FALSE)
  }

  type <- typeof(x)
  switch(
    type,
    NULL = return("null"),
    logical = if (vec_is_unspecified(x)) {
      return("unspecified")
    } else {
      return(type)
    },
    integer = ,
    double = ,
    character = ,
    raw = ,
    list = return(type)
  )

  stop(sprintf("Unimplemented type <%s>.", type), call. = FALSE)
}

vec_is_unspecified <- function(x) {
  !is.object(x) &&
    typeof(x) == "logical" &&
    length(x) &&
    all(vapply(x, identical, logical(1), NA))
}

.rlang_vctrs_unspecified <- function(x = NULL) {
  structure(
    rep(NA, length(x)),
    class = "rlang_unspecified"
  )
}

.rlang_vctrs_s3_method <- function(generic, class, env = parent.frame()) {
  fn <- get(generic, envir = env)

  ns <- asNamespace(topenv(fn))
  tbl <- ns$.__S3MethodsTable__.

  for (c in class) {
    name <- paste0(generic, ".", c)
    if (exists(name, envir = tbl, inherits = FALSE)) {
      return(get(name, envir = tbl))
    }
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      return(get(name, envir = globalenv()))
    }
  }

  NULL
}



# zeallot ----------

`%<-%` <- function(lhs, value) {
  lhs <- substitute(lhs)
  env <- caller_env()

  if (!is_call(lhs, "c")) {
    abort("The left-hand side of `%<-%` must be a call to `c()`.")
  }

  vars <- as.list(lhs[-1])

  if (length(value) != length(vars)) {
    abort("The left- and right-hand sides of `%<-%` must be the same length.")
  }

  for (i in seq_along(vars)) {
    var <- vars[[i]]
    if (!is_symbol(var)) {
      abort(paste0("Element ", i, " of the left-hand side of `%<-%` must be a symbol."))
    }

    env[[as_string(var)]] <- value[[i]]
  }

  invisible(value)
}



# nocov env