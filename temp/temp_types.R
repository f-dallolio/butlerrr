
is_base_type <- function(x, class = NULL, type = NULL, n = NULL){
  if(is.null(n)) {
    n_out <- TRUE
  } else {
    n_out <- length(x) == n
  }
  if(is.null(type)){
    out_type <- typeof(x) %in% c("logical", "integer", "double", "complex",
                                 "character", "raw", "list")
  }
  if(is.null(class)) {
    out_class <- class(x) %in%  c("logical", "integer", "double", "complex",
                                       "character", "raw", "numeric")
  }
  all(n_out, out_type, out_class)
}

is_base_scalar <- function(x, class = NULL, type = NULL){
  is_base_type(x, class = class, type = type, n = 1)
}



langs(.x = letters, .out_type = "call")


class(raw(3))
class(x)
typeof(c("a", "b"))
!is.objletters!is.object(c(a = 1))

"NULL",

"closure" (function), "special" and "builtin" (basic functions and operators),

"environment",

"symbol",
"language",

"S4" (some S4 objects)

"pairlist",
"promise",
"object",

"char",
"...",
"any",
"expression",
"externalptr",
"bytecode"
"weakref").

vctrs::vec_ptype(numeric(1)) |> identical(vctrs::vec_ptype(integer(1)) )
