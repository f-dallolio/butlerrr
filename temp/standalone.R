standalone <- function(repo_spec) {
  json <- gh::gh(
    "/repos/{repo_spec}/contents/{path}",
    repo_spec = repo_spec,
    ref = NULL,
    .api_url = NULL,
    path = "R/"
  )

  names <- vapply(json, getElement, character(1), "name")
  urls <- vapply(json, getElement, character(1), "download_url")

  pos <- grepl("^standalone-", names)

  names <- names[pos]
  urls <- urls[pos]
  choices <- gsub("^standalone-|.[Rr]$", "", names)

  if(length(choices) == 0){
    msg <- sprintf("No standalone files found in %s.", repo_spec)
    stop(msg)
  }
  purrr_choice <- choices == "purrr"
  calls <- parse(text = readLines(con = url(urls[purrr_choice])))

  calls_chr <- as.list(calls) |>
    sapply(\(x) paste(deparse(x), collapse = "\n"))

  file.create("R/standalone-purrr.R")
  cat(sprintf("#' @export \n %s \n ", calls_chr), sep = "\n", file = "R/standalone-purrr.R")

  purrr_env <- new.env()
  eval(calls, envir = purrr_env)
  lapply(names(purrr_env), get, envir = purrr_env) |>
    setNames(names(purrr_env))


  vapply(calls, is.function, logical(1))

}
f(repo_spec)
