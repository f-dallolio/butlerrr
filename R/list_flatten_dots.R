list_flatten_dots <- function(x, .named = FALSE){
  i <- which(names(x) == "...")
  pre_dots <- x[seq2(1, i - 1)]
  post_dots <- x[seq2(i + 1, length(x))]
  dots <- x[[i]]
  nms_dots <- names(dots)
  nms_flag <- nms_dots %in% names(x[-1])
  nms_dots[nms_flag] <- paste("...", nms_dots[nms_flag], sep = "_")

  if(.named){
    nms_new <- paste("...",seq_along(x[["..."]]), sep = "_")
    if(is.null(nms_dots)){
      nms_dots <- nms_new
    } else {
      nms_dots[nms_dots == ""] <-  nms_new[nms_dots == ""]
    }
  }
  names(dots) <- nms_dots
  append(pre_dots, append(dots, post_dots))
}
