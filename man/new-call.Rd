% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_call.R
\name{new-call}
\alias{new-call}
\alias{new_call_simple}
\alias{call_simple}
\title{Create new named calls}
\usage{
new_call_simple(.fn, .args = NULL, .ns = NULL, .private = NULL)

call_simple(.fn, ..., .ns = NULL, .private = NULL)
}
\arguments{
\item{.fn}{a string or a symbol.}

\item{.args}{a list.}

\item{.ns}{NULL, TRUE, or FALSE.}

\item{.private}{NULL, TRUE, or FALSE.}
}
\value{
a named call ("call_simple").
}
\description{
Create new named calls
}
