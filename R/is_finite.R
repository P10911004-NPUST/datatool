`_is_finite` <- function(x = double())
{
    stopifnot(is.numeric(x))
    is.finite(x)
}


x <- c(1, 2i, Inf, NA_complex_, NA_integer_, NA_real_, NaN, -8)
is.finite(x)

`_is_finite`("a")

is.numeric(NA)
