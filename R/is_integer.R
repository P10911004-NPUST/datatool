#' @title
#' Identify Integer Values
#'
#' @description
#' A wrapper for the `base::is.integer()`. By default, NOT including `NA_integer_`.
#'
#' @param x An atomic or recursive vector (i.e., c(), matrix(), list(), or data.frame()).
#' @param include_NA_integer Logical (default: FALSE). Consider `NA_integer_` as an integer value?
#'
#' @return A logical vector which has the same length and/or dimensions with `x`.
#' @export
#'
#' @examples
#' test <- list(
#'     int = 1:5,
#'     dbl = c(1.2, -3.3, 7.5, 2.9),
#'     char = list(LETT = LETTERS[1:3], lett = letters[1:3]),
#'     cplx = matrix(complex(real = 1:12, imaginary = 3:9), nrow = 3),
#'     bool = data.frame(true = rep(TRUE, 5), false = rep(FALSE, 5)),
#'     na = list(na_bool = NA,
#'               na_char = NA_character_,
#'               na_cplx = NA_complex_,
#'               na_int = NA_integer_,
#'               na_dbl = NA_real_),
#'     special = list(true = TRUE, false = FALSE, nan = NaN, null = NULL)
#' )
#' is_integer(test)
is_integer <- function(x, include_NA_integer = FALSE)
{
    UseMethod("is_integer")
}
#' @export
is_integer.default <- function(x, include_NA_integer = FALSE)
{
    .is_integer_vector(x, include_NA_integer)
}

#' @export
is_integer.matrix <- function(x, include_NA_integer = FALSE)
{
    .is_integer_matrix(x, include_NA_integer)
}

#' @export
is_integer.list <- function(x, include_NA_integer = FALSE)
{
    .is_integer_list(x, include_NA_integer)
}

#' @export
is_integer.data.frame <- function(x, include_NA_integer = FALSE)
{
    .is_integer_dataframe(x, include_NA_integer)
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Internal functions ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
.is_integer_vector <- function(vct, include_NA_integer = FALSE)
{
    if ( is.null(vct) | length(vct) == 0 )
        return(FALSE)

    vapply(
        X = vct,
        FUN = function(x)
        {
            ret <- is.integer(x)
            if ( ! include_NA_integer )
                ret <- ret && !is.na(x)
            return(ret)
        },
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
    )
}


.is_integer_matrix <- function(mat, include_NA_integer = FALSE)
{
    if ( is.null(mat) | length(mat) == 0 )
        return(FALSE)

    shape <- dim(mat)
    ret <- mapply(
        FUN = function(x, .include_NA)
        {
            ret <- is.integer(x)
            if ( isFALSE(.include_NA) )
                ret <- ret && !is.na(x)
            return(ret)
        },
        .include_NA = include_NA_integer,
        x = mat,
        USE.NAMES = TRUE
    )
    ret <- matrix(ret, nrow = shape[1], dimnames = dimnames(mat))
    return(ret)
}


.is_integer_list <- function(lst, include_NA_integer = FALSE)
{
    if (is.null(lst) | length(lst) == 0)
        return(FALSE)

    if ( is.null(dim(lst)) & inherits(lst, "list") )
    {
        ret <- lapply(
            X = lst,
            FUN = function(x, .include_NA)
            {
                if (is.null(x) | length(x) == 0)
                    return(FALSE)

                .is_integer_list(x, include_NA_integer = .include_NA)
            },
            .include_NA = include_NA_integer
        )
    }

    if (is.atomic(lst) & is.null(dim(lst)))
        ret <- .is_integer_vector(lst, include_NA_integer)

    if (inherits(lst, "matrix"))
        ret <- .is_integer_matrix(lst, include_NA_integer)

    if (inherits(lst, "data.frame"))
        ret <- .is_integer_dataframe(lst, include_NA_integer)

    return(ret)
}


.is_integer_dataframe <- function(df, include_NA_integer = FALSE)
{
    ret <- vapply(
        X = df,
        FUN = function(x, .include_NA)
        {
            .is_integer_vector(x, include_NA_integer = .include_NA)
        },
        .include_NA = include_NA_integer,
        FUN.VALUE = logical(nrow(df)),
        USE.NAMES = TRUE
    )
    return(as.data.frame(ret))
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Testing
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
if (FALSE)
{
    is_integer(test_vector)
    is_integer(test_matrix)
    is_integer(test_list, include_NA_integer = TRUE)
    is_integer(test_dataframe)
}
