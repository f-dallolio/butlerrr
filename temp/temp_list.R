new_lang_list <- function(x = list(), env = emptyenv()){
  if(!is.vector(x)) x <- list(x)
  stopifnot(all2(x, is_symbolic))
  unique_types <- lang_unique_types(x)
  n_types <- length(unique_types)
  stopifnot(n_types %in% c(0, 1, 2))
  class <- "lang_list"
  if(n_types == 1){
    if(unique_types == "symbol"){
      lang_type <- "symbols"
    } else if(unique_types == "call"){
      lang_type <- "calls"
    }
  } else {
    lang_type <- "symbolics"
  }

  nms <- names(x)
  if(!is.null(nms)){
    nms[is.na(nms)] <- ""
  }

  out <- structure(x, env = env, lang_type = lang_type, class = c(class, "list"))
  out
}

# format.lang_list <- function(x){
#   out <- names(symbolic_auto_name(x, .name_all = TRUE))
#   names(out) <- names(x)
#   out
# }


is_lang_list <- function(x, lang_type = NULL){
  out <- inherits(x, "lang_list")
  if(!is.null(lang_type) && out){
    choices  <-  c("symbolics", "symbolic", "symc", "symcs",
                "symbols", "symbol", "syms", "sym",
                "calls", "call")
    lang_type <- match.arg(arg = lang_type, choices = choices)
    if(lang_type %in% c("symbolics", "symbolic", "symc", "symcs")) {
      lang_type <- "symbolics"
    } else if(lang_type %in% c("symbols", "symbol", "syms", "sym")) {
      lang_type <- "symbols"
    } else {
      lang_type <- "calls"
    }
    return(attr(x, "lang_type") == lang_type)
  }
  out
}
is_symbolic_list <- function(x){
  is_lang_list(x, lang_type = "symbolics")
}
is_sym_list <- function(x){
  is_lang_list(x, lang_type = "symbols")
}
is_call_list <- function(x){
  is_lang_list(x, lang_type = "calls")
}

print.lang_list <- function(x){
  env <- environmentName(attr(x, "env"))
  lang_type <- attr(x, "lang_type")

  type_nms <- format(c("lang_type: ", "env_name: "))
  type <- c(dQuote(lang_type, q = FALSE), dQuote(env, q = FALSE))
  type_out <- paste0(type_nms, type)

  sep_line <- strrep("-", max(nchar(type_out, type = "width")))

  n <- length(x)
  nms <- names(x)

  # nms_pos <- which(nms == "")
  # if(length(nms_pos)){
  #   nms[nms_pos] <- paste0("[[", nms_pos, "]]")
  # }



  header <- sprintf("<lang_list[%i]>\n", n)
  cat(header, "\n")

  if(length(x) == 0){
    out_type <- gsub("s$", "",lang_type)
    out <- paste0(out_type,"[0]")
    names(out) <- ""
    cat(out, "\n")
  } else {
    out <- names(symbolic_auto_name(x, .name_all = TRUE))
    names(out) <- nms
    print(out, quote = FALSE)
  }

  cat(sep_line, "\n")
  cat(type_out, sep = "\n")
  invisible(out)
}
new_lang_list(rep(alist(mean()), 30))
new_lang_list()

new_calls_list <- function(x, ns = NULL, env = NULL){
  if(is.null(ns)){
    if(is.null(env)){ env <- parent.frame() }
    stopifnot(is.environment(env))
  } else {
    stopifnot(is.character(ns) && length(ns) == 1)
    env <- asNamespace(ns)
  }
  stopifnot( butlerrr::all2(x, ~is.call(.x) && !butlerrr::is_ns_sym(.x)) )
  out <- structure(x, env = env, class = c("calls"))
  invisible(out)
}

format.calls <- function(x){
  out <- names(butlerrr::symbolic_auto_name(x, .name_all = TRUE))
  names(out) <- names(x)
  out
}

print.calls <- function(x){
  out <- names(butlerrr::symbolic_auto_name(x, .name_all = TRUE))
  names(out) <- names(x)
  cat(paste0("<", typeof(x), "> <<call[", length(x),"]>>"), sep = "\n")
  print(out, quote = FALSE)
  cat(paste0("<env: ", environmentName(attr(x, "env")), ">", sep = "\n"))
  invisible(out)
}



lang_type <- function(x){
  stopifnot(is_symbolic(x))
  if(butlerrr::is_ns_sym(x) || typeof(x) == "symbol"){
    "symbol"
  } else if(is.call(x)){
    "call"
  } else {
    typeof(x)
  }
}

lang_types <- function(x, unique = FALSE){
  if(is.vector(x)){
    out <- vapply(x, lang_type, character(1))
  } else {
    out <- lang_type(x)
  }
  if(unique){
    out <- unique(out)
  }
  out
}

lang_unique_types <- function(x){
  lang_types(x, unique = TRUE)
}

is_list <- function(x, obj_classes = NULL, n =NULL, verbose = TRUE){
  out_list <- is.list(x)
  if(is.null(n) && is.null(obj_classes)) return(out_list)

  msgs <- c(n = NA, obj_classes = NA)
  if(!is.null(n)){
    stopifnot(is.integer(as.integer(n)) && length(n) == 1)
    if(n != length(x)){
      msgs["n"] <- paste0("Length of input is ", length(x), ". It must be ", n, ".")
    }
  }

  if(!is.null(obj_classes)){
    stopifnot(is.character(obj_classes))
    if("symbolic"%in% obj_classes) obj_classes["symbolic"] <- c("symbol", "call")
    objs <- objs_class(x, unique = FALSE)
    not <- ! objs %in% obj_classes
    if(any(not)){
      plural <- length(which(not)) > 1
      msgs["obj_classes"] <- paste0(
        "The \"obj_class\" of input element",
        if(plural) "s " else " ",
        butlerrr::str_oxford(which(not)),
        # if(plural) " are " else " is ",
        " is not ",
        butlerrr::str_oxford(
          butlerrr::str_embrace(obj_classes, "\""),
          or = TRUE
        ),
        "."
      )
    }
  }
  not <- na.omit(msgs)
  if(length(not) > 0){
    if(verbose){
      hint <- paste0("Hint", if(length(not) > 1) "s:" else ":")
      cat(hint, str_itemize(not), sep = "\n")
    }
    FALSE
  } else {
    TRUE
  }
}


print.lang_list <- function(x, print_as_list = FALSE){
  env <- attr(x, "env")
  lang_type <- attr(x, "lang_type")
  n <- length(x)
  header <- sprintf("<list_of<%s>[%i]>", lang_type, n)
  footer <- sprintf("env_name: %s", dQuote(environmentName(env), q = FALSE))
  footer_sep <- strrep("-", nchar(footer, type = "width"))

  out <- names(symbolic_auto_name(x, .name_all = TRUE, .names_type = "full"))
  nms <- paste0("[[", seq_along(x), "]]")
  nms0 <- names(x)
  nms0[is.na(nms0)] <- ""
  if(!is.null(nms0)){
    nms[nms0 != ""] <- nms0[nms0 != ""]
  }
  names(out) <- nms
  cat(header, "\n")
  if(print_as_list){
    print(unlist(x, use.names = TRUE))
  } else {
    print(out, quote = FALSE, print.gap = 2)
  }
  cat(footer_sep, footer, sep = "\n")
}

