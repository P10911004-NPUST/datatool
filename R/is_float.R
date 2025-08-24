#' @title
#' Identify Floating-Point Numbers
#'
#' @description
#' A wrapper for `base::is.double()`. By default, `is_float()` doesn't consider
#' `Inf`, `NaN`, and `NA_real_` as floating-point value (so return `FALSE`),
#' which is different from `base::is.double()`.
#'
#' @param x An atomic or recursive vector (i.e., c(), matrix(), list(), or data.frame()).
#' @param include_Inf Logical (default: FALSE). Consider `Inf` as float?
#' @param include_NaN Logical (default: FALSE). Consider `NaN` as float?
#' @param include_NA_real Logical (default: FALSE). Consider `NA_real_` as float?
#'
#' @return A logical vector which has the same length and/or dimensions with `x`.
#' @export
#'
#' @examples
#' test <- list(
#'     dbl = c(1.2, -3.3, 7.5, 2.9),
#'     int = 1:5,
#'     char = c(LETT = LETTERS[1:3], lett = letters[1:3]),
#'     cplx = matrix(complex(real = 1:12, imaginary = 3:9), nrow = 3),
#'     bool = data.frame(true = rep(TRUE, 5), false = rep(FALSE, 5)),
#'     na = list(NA, NA_character_, NA_complex_, na_int = NA_integer_, NA_real_),
#'     special = list(TRUE, FALSE, NaN, NULL)
#' )
#' is_float(test)
is_float <- function(
        x,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    UseMethod("is_float")
}

#' @export
is_float.default <- function(
        x,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    .is_float_vector(x, include_Inf, include_NaN, include_NA_real)
}

#' @export
is_float.matrix <- function(
        x,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    .is_float_matrix(x, include_Inf, include_NaN, include_NA_real)
}

#' @export
is_float.list <- function(
        x,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    .is_float_list(x, include_Inf, include_NaN, include_NA_real)
}

#' @export
is_float.data.frame <- function(
        x,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    .is_float_dataframe(x, include_Inf, include_NaN, include_NA_real)
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Internal functions ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
.is_float_vector <- function(
        vct,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    if ( is.null(vct) | length(vct) == 0 )
        return(FALSE)

    vapply(
        X = vct,
        FUN = function(x, .include_Inf, .include_NaN, .include_NA_real)
        {
            ret <- is.double(x)
            if ( ! .include_Inf ) ret <- ret && !is.infinite(x)
            # `NaN` also belongs to double type
            if ( ! .include_NaN ) ret <- ret && !is.nan(x)
            # In case of `c(NaN, NA)`, `NA` will be coerced to `NA_real_`
            # is.na() cannot discriminate c(NaN, NA), use identical() instead
            if ( ! .include_NA_real ) ret <- ret && !identical(x, NA_real_)
            return(ret)
        },
        .include_Inf = include_Inf,
        .include_NaN = include_NaN,
        .include_NA_real = include_NA_real,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
    )
}


.is_float_matrix <- function(
        mat,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    if ( is.null(mat) | length(mat) == 0 )
        return(FALSE)

    shape <- dim(mat)
    ret <- mapply(
        FUN = function(x, .include_Inf, .include_NaN, .include_NA_real)
        {
            ret <- is.double(x)
            if ( ! .include_Inf ) ret <- ret && !is.infinite(x)
            # `NaN` also belongs to double type
            if ( ! .include_NaN ) ret <- ret && !is.nan(x)
            # In case of `c(NaN, NA)`, `NA` will be coerced to `NA_real_`
            if ( ! .include_NA_real ) ret <- ret && !identical(x, NA_real_)
            return(ret)
        },
        x = mat,
        .include_Inf = include_Inf,
        .include_NaN = include_NaN,
        .include_NA_real = include_NA_real,
        USE.NAMES = TRUE
    )
    ret <- matrix(ret, nrow = shape[1], dimnames = dimnames(mat))
    return(ret)
}


.is_float_list <- function(
        lst,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    if (is.null(lst) | length(lst) == 0)
        return(FALSE)

    if ( is.null(dim(lst)) & inherits(lst, "list") )
    {
        ret <- lapply(
            X = lst,
            FUN = function(x, .include_Inf, .include_NaN, .include_NA_real)
            {
                if (is.null(x) | length(x) == 0)
                    return(FALSE)

                .is_float_list(x, .include_Inf, .include_NaN, .include_NA_real)
            },
            .include_Inf = include_Inf,
            .include_NaN = include_NaN,
            .include_NA_real = include_NA_real
        )
    }

    if (is.atomic(lst) & is.null(dim(lst)))
        ret <- .is_float_vector(lst, include_Inf, include_NaN, include_NA_real)

    if (inherits(lst, "matrix"))
        ret <- .is_float_matrix(lst, include_Inf, include_NaN, include_NA_real)

    if (inherits(lst, "data.frame"))
        ret <- .is_float_dataframe(lst, include_Inf, include_NaN, include_NA_real)

    return(ret)
}


.is_float_dataframe <- function(
        df,
        include_Inf = FALSE,
        include_NaN = FALSE,
        include_NA_real = FALSE
) {
    ret <- vapply(
        X = df,
        FUN = function(x)
        {
            .is_float_vector(x, include_Inf, include_NaN, include_NA_real)
        },
        FUN.VALUE = logical(nrow(df)),
        USE.NAMES = TRUE
    )
    return(as.data.frame(ret))
}
