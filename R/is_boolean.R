#' @title
#' Identify Boolean Values
#'
#' @description
#' A wrapper for the `base::is.logical()`, `base::isTRUE()`, and `base::isFALSE(`).
#' Unlike the base functions, they exclude `NA` from being considered logical by default.
#'
#' @param x An atomic or recursive vector (i.e., c(), matrix(), list(), or data.frame()).
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
#' is_TRUE(test)
#' is_FALSE(test)
#' is_boolean(test)
is_boolean <- function(x)
{
    UseMethod("is_boolean")
}
#' @export
is_boolean.default <- function(x) .is_boolean_vector(x)
#' @export
is_boolean.matrix <- function(x) .is_boolean_matrix(x)
#' @export
is_boolean.list <- function(x) .is_boolean_list(x)
#' @export
is_boolean.data.frame <- function(x) .is_boolean_dataframe(x)


#' @rdname is_boolean
#' @export
is_logical <- function(x)
{
    UseMethod("is_logical")
}
#' @export
is_logical.default <- function(x) .is_boolean_vector(x)
#' @export
is_logical.matrix <- function(x) .is_boolean_matrix(x)
#' @export
is_logical.list <- function(x) .is_boolean_list(x)
#' @export
is_logical.data.frame <- function(x) .is_boolean_dataframe(x)


#' @rdname is_boolean
#' @export
is_TRUE <- function(x)
{
    UseMethod("is_TRUE")
}
#' @export
is_TRUE.default <- function(x) .is_boolean_vector(x, only_TRUE = TRUE)
#' @export
is_TRUE.matrix <- function(x) .is_boolean_matrix(x, only_TRUE = TRUE)
#' @export
is_TRUE.list <- function(x) .is_boolean_list(x, only_TRUE = TRUE)
#' @export
is_TRUE.data.frame <- function(x) .is_boolean_dataframe(x, only_TRUE = TRUE)


#' @rdname is_boolean
#' @export
is_FALSE <- function(x) UseMethod("is_FALSE")
#' @export
is_FALSE.default <- function(x) .is_boolean_vector(x, only_FALSE = TRUE)
#' @export
is_FALSE.matrix <- function(x) .is_boolean_matrix(x, only_FALSE = TRUE)
#' @export
is_FALSE.list <- function(x) .is_boolean_list(x, only_FALSE = TRUE)
#' @export
is_FALSE.data.frame <- function(x) .is_boolean_dataframe(x, only_FALSE = TRUE)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Internal functions ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
.is_boolean_vector <- function(vct, only_TRUE = FALSE, only_FALSE = FALSE)
{
    if ( is.null(vct) | length(vct) == 0 )
        return(FALSE)

    vapply(
        X = vct,
        FUN = function(x, .only_TRUE, .only_FALSE)
        {
            if ( is.null(x) | length(x) == 0 )
                return(FALSE)

            if (.only_TRUE)
                isTRUE(x)
            else if (.only_FALSE)
                isFALSE(x)
            else
                # is.logical(x) & !is.na(x)
                isTRUE(x) || isFALSE(x)
        },
        .only_TRUE = only_TRUE,
        .only_FALSE = only_FALSE,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
    )
}


.is_boolean_matrix <- function(mat, only_TRUE = FALSE, only_FALSE = FALSE)
{
    if ( is.null(mat) | length(mat) == 0 )
        return(FALSE)

    shape <- dim(mat)
    ret <- mapply(
        FUN = function(x)
        {
            if (is.null(x)) return(FALSE)
            if (only_TRUE)
                isTRUE(x)
            else if (only_FALSE)
                isFALSE(x)
            else
                isTRUE(x) || isFALSE(x)
        },
        x = mat,
        USE.NAMES = TRUE
    )
    ret <- matrix(ret, nrow = shape[1], dimnames = dimnames(mat))
    return(ret)
}


.is_boolean_list <- function(lst, only_TRUE = FALSE, only_FALSE = FALSE)
{
    if ( is.null(lst) | length(lst) == 0 )
        return(FALSE)

    if ( is.null(dim(lst)) & inherits(lst, "list") )
    {
        ret <- lapply(
            X = lst,
            FUN = function(x, .only_TRUE, .only_FALSE)
            {
                if ( is.null(lst) | length(lst) == 0 ) return(FALSE)
                .is_boolean_list(x, .only_TRUE, .only_FALSE)
            },
            .only_TRUE = only_TRUE,
            .only_FALSE = only_FALSE
        )
    }

    if (is.atomic(lst) & is.null(dim(lst)))
        ret <- .is_boolean_vector(lst, only_TRUE, only_FALSE)

    if (inherits(lst, "matrix"))
        ret <- .is_boolean_matrix(lst, only_TRUE, only_FALSE)

    if (inherits(lst, "data.frame"))
        ret <- .is_boolean_dataframe(lst, only_TRUE, only_FALSE)

    return(ret)
}


.is_boolean_dataframe <- function(df, only_TRUE = FALSE, only_FALSE = FALSE)
{
    ret <- vapply(
        X = df,
        FUN = function(x, .only_TRUE, .only_FALSE)
        {
            .is_boolean_vector(x, .only_TRUE, .only_FALSE)
        },
        .only_TRUE = only_TRUE,
        .only_FALSE = only_FALSE,
        FUN.VALUE = logical(nrow(df)),
        USE.NAMES = TRUE
    )
    return(as.data.frame(ret))
}





