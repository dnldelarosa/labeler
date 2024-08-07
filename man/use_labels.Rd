% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Labels.R
\name{use_labels}
\alias{use_labels}
\title{Use the data labels of a variable instead of its values
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}}
\usage{
use_labels(tbl, dict, vars = NULL, ignore_case = F, check = TRUE, warn = TRUE)
}
\arguments{
\item{tbl}{\link{data.frame}: Data.frame with the data}

\item{dict}{\link{data.frame}: Dictionary with all the data labels to use if they
have not yet been assigned. See \code{Details}}

\item{vars}{\link{character}: If specified, only labels are assigned to those variables}

\item{ignore_case}{\link{logical}: Indicate if case sensitive should be ignored}

\item{check}{\link{logical}: If TRUE (default), the function will check if values
present in variable are valid data labels in dictionary.}

\item{warn}{\link{logical}: Indicate if warnings should be shown.}
}
\value{
The data supplied in the \code{tbl} argument, but instead of values
using the corresponding data labels
}
\description{
Use the data labels of a variable instead of its values
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\details{
This function assumes that the data labels have been assigned to the
data before, unless a \code{dict} is supplied, in which case the latter
will be used to assign and use the data labels.
}
\examples{
\dontrun{
dict <- list(
  SEX = list(
    lab = "Sex of the person",
    labs = c(
      "Man" = 1,
      "Woman" = 2
    )
  )
)
enft <- data.frame(SEX = c(1, 2))
enft
use_labels(enft, dict = dict)
}
}
\seealso{
Data labels \code{vignette("labeler", package = "labeler")}
}
