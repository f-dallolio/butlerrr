#' Collect, evaluate, and/or splice dots(`...`)
#'
#' @param .eval .
#' @param .splice .
#' @param .env .
#'
#' @return a list.
#' @name new-dots
NULL
#' @rdname new-dots
#' @export
get_dots <- function(){
  eval(substitute(alist(...), env = parent.frame()))
}
#' @rdname new-dots
#' @export
dots_eval <- function(..., .splice = FALSE, .env = parent.frame()){
  dots <- get_dots()
  lapply(dots, eval, envir = .env)
}
#' @rdname new-dots
#' @export
dots_splice <- function(..., .env = parent.frame()){
  unlist(dots_eval(..., .env = .env), use.names = TRUE)
}
#'
#' # Collect dots as symbolic objects
#' #'
#' #' @param .lang_type .
#' #' @param .named .
#' #' @param .name_all .
#' #' @param .out_as_list .
#' #'
#' #' @return a list.
#' #' @name dots-lang
#' NULL
#' #' @rdname dots-lang
#' #' @export
#' dots_lang <- function(.lang_type = NULL,
#'                           .named = FALSE,
#'                           .name_all = FALSE,
#'                           .dots_env = NULL,
#'                           .out_as_list = FALSE) {
#'   stopifnot(inside_fn())
#'
#'   caller_fn <- sys.function(sys.parent())
#'   caller_call <- sys.call(sys.parent())
#'
#'   call <- match.call(caller_fn, caller_call, expand.dots = FALSE)
#'   dots_pos <- which(names(call) == "...")
#'   names(call)[dots_pos] <- ""
#'   call[[dots_pos]] <- quote(`...`)
#'
#'   .dots_env <- if_null(.dots_env, parent.frame())
#'   dots <- get_dots(env = .dots_env)
#'   dots <- lapply(dots, as_symbolic_obj)
#'   dots_lang_type <- "symbolics"
#'
#'   if(!is.null(.lang_type)){
#'     dots_lang_type <- choose_lang_type(.lang_type)
#'     if(dots_lang_type == "symbols") dots <- lapply(dots, \(x) as.list(x)[[1]])
#'     if(dots_lang_type == "named calls") dots <- lapply(dots, \(x) as.call(as.list(x)))
#'   }
#'
#'   if(.named) dots <- symbolic_auto_name(dots)
#'
#'   structure(dots,
#'             lang_type = dots_lang_type,
#'             caller_call = call,
#'             list_output = if(.out_as_list) TRUE else NULL,
#'             class = "dots_lang")
#' }
#' #' @rdname dots-lang
#' #' @export
#' print.dots_lang <- function(x,
#'                                 .out_as_list = attr(x, "list_output")){
#'
#'   call <- attr(x, "caller_call")
#'   dots_lang_type <- attr(x, "lang_type")
#'
#'   if(dots_lang_type == "symbolics") {
#'     dots_lang_type <- "\"symbolic\" objects"
#'   } else {
#'     dots_lang_type <- dQuote(dots_lang_type)
#'   }
#'
#'   header <- sprintf("<dots_list[%i]> ... as %s", length(x), dots_lang_type)
#'
#'   if(!is.null(.out_as_list)){
#'     cat(header, "\n")
#'     print(unlist(x, use.names = TRUE))
#'     return(invisible(x))
#'   }
#'
#'   out <- symbolic_auto_name(x, .name_all = TRUE, .names_type = "full")
#'   out <- names(out)
#'
#'   nms_id <- paste0("[[", seq_along(x), "]]")
#'   nms_out <- names(x)
#'   if(is.null(nms_out)) nms_out <- rep("", length(x))
#'
#'   named_out <- nms_out != ""
#'   nms_out[named_out] <- dQuote(nms_out[named_out], q = FALSE)
#'   nms_out[!named_out] <- nms_id[!named_out]
#'
#'   footer_nm <- "caller_call:  "
#'   nchr <- nchar(footer_nm, "width")
#'   tot_wdt <- 0.7 * min(80, getOption("width"))
#'   wdt <- tot_wdt - nchr
#'   chr_call <- capture.output(print(call, width = wdt))
#'   footer <- paste0(
#'     format(c(footer_nm, rep(" ", length(chr_call) - 1))),
#'     chr_call)
#'   footer_sep <- strrep("-", max(nchar(footer, "width"), tot_wdt))
#'
#'   cat(header, "\n")
#'   print(setNames(out, nms_out), quote = FALSE, print.gap = 2)
#'   cat(footer_sep, footer, sep = "\n")
#'
#'   invisible(x)
#' }
#'
#' choose_lang_type <- function(x){
#'   choices  <-  c("symbolics", "symbolic", "symc", "symcs",
#'                  "symbols", "symbol", "syms", "sym", "name",
#'                  "calls", "call", "language")
#'   lang_type <- match.arg(arg = x, choices = choices)
#'   if(lang_type %in% c("symbols", "symbol", "syms", "sym", "name")) {
#'     return("symbols")
#'   } else if(lang_type %in% c("calls", "call", "language")){
#'     return("named calls")
#'   } else {
#'     return("symbolics")
#'   }
#' }
