% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_float.R
\name{is_float}
\alias{is_float}
\title{Identify Floating-Point Numbers}
\usage{
is_float(x, include_Inf = FALSE, include_NaN = FALSE, include_NA_real = FALSE)
}
\arguments{
\item{x}{An atomic or recursive vector (i.e., c(), matrix(), list(), or data.frame()).}

\item{include_Inf}{Logical (default: FALSE). Consider \code{Inf} as float?}

\item{include_NaN}{Logical (default: FALSE). Consider \code{NaN} as float?}

\item{include_NA_real}{Logical (default: FALSE). Consider \code{NA_real_} as float?}
}
\value{
A logical vector which has the same length and/or dimensions with \code{x}.
}
\description{
A wrapper for \code{base::is.double()}. By default, \code{is_float()} doesn't consider
\code{Inf}, \code{NaN}, and \code{NA_real_} as floating-point value (so return \code{FALSE}),
which is different from \code{base::is.double()}.
}
\examples{
test <- list(
    dbl = c(1.2, -3.3, 7.5, 2.9),
    int = 1:5,
    char = c(LETT = LETTERS[1:3], lett = letters[1:3]),
    cplx = matrix(complex(real = 1:12, imaginary = 3:9), nrow = 3),
    bool = data.frame(true = rep(TRUE, 5), false = rep(FALSE, 5)),
    na = list(NA, NA_character_, NA_complex_, na_int = NA_integer_, NA_real_),
    special = list(TRUE, FALSE, NaN, NULL)
)
is_float(test)
}
