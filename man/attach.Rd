% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attach_and_with.R
\name{attach}
\alias{attach}
\title{this just implements attach for an edsurvey.data.frame and a light.edsurvey.data.frame}
\usage{
attach(
  what,
  pos = 2L,
  name = deparse1(substitute(what), backtick = FALSE),
  warn.conflicts = TRUE
)
}
\arguments{
\item{what}{equivalent to `what` in base::attach, but can also be an edsurvey.data.frame}

\item{pos}{equivalent to `pos` in base::attach}

\item{name}{equivalent to `name` in base::attach}

\item{warn.conflicts}{equivalent to `warn.conflicts` in base::attach}
}
\description{
this just implements attach for an edsurvey.data.frame and a light.edsurvey.data.frame
}
\author{
Blue Webb
}
