make_call <- function(fn, args = NULL){
  call_list <- vector("list", length = length(args) + 1)
  call_list[[1]] <- enlang(fn)
  call_list[-1] <- args
  as.call(call_list)
}
