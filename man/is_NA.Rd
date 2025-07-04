% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_NA.R
\name{is_NA}
\alias{is_NA}
\title{Identify \code{NA} (not available)}
\usage{
is_NA(x)
}
\arguments{
\item{x}{An atomic or recursive vector (i.e., c(), matrix(), list(), or data.frame()).}
}
\value{
A logical vector which has the same length and/or dimensions with \code{x}.
}
\description{
A wrapper for the \code{base::is.na()}.
Identify \code{NA} (boolean), \code{NA_character_}, \code{NA_complex_}, \code{NA_integer_}, and \code{NA_real_},
but exclude \code{NaN}.
Similar to \code{base::is.na(x) & !base::is.nan(x)}.
}
\examples{
test <- list(
    int = 1:5,
    dbl = c(1.2, -3.3, 7.5, 2.9),
    char = list(LETT = LETTERS[1:3], lett = letters[1:3]),
    cplx = matrix(complex(real = 1:12, imaginary = 3:9), nrow = 3),
    bool = data.frame(true = rep(TRUE, 5), false = rep(FALSE, 5)),
    na = list(na_bool = NA,
              na_char = NA_character_,
              na_cplx = NA_complex_,
              na_int = NA_integer_,
              na_dbl = NA_real_),
    special = list(true = TRUE, false = FALSE, nan = NaN, null = NULL)
)
is_NA(test)
}
