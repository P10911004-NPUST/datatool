#' Identify Integer Values
#'
#' @description
#' A wrapper for the base::is.integer().
#'
#' @param x An atomic or recursive vector (i.e., c(), matrix(), list(), or data.frame()).
#'
#' @return A logical vector which has the same length and dimensions with `x`.
#' @export
#'
#' @examples
#' test <- list(
#'     num = c(1.2, -3.3, 7.5, 2.9),
#'     int = 1:5,
#'     char = c(LETT = LETTERS[1:3], lett = letters[1:3]),
#'     cplx = matrix(complex(real = 1:12, imaginary = 3:9), nrow = 3),
#'     bool = data.frame(true = rep(TRUE, 5), false = rep(FALSE, 5)),
#'     na = list(NA, NA_character_, NA_complex_, na_int = NA_integer_, NA_real_),
#'     special = list(TRUE, FALSE, NaN, NULL)
#' )
#' is_integer(test)
is_integer <- function(x)
{
    UseMethod("is_integer")
}
#' @export
is_integer.default <- function(x) .is_integer_vector(x)
#' @export
is_integer.matrix <- function(x) .is_integer_matrix(x)
#' @export
is_integer.list <- function(x) .is_integer_list(x)
#' @export
is_integer.data.frame <- function(x) .is_integer_dataframe(x)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Internal functions ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
.is_integer_vector <- function(vct)
{
    if ( is.null(vct) | length(vct) == 0 ) return(FALSE)
    vapply(
        X = vct,
        FUN = is.integer,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
    )
}


.is_integer_matrix <- function(mat)
{
    if ( is.null(mat) | length(mat) == 0 ) return(FALSE)
    if (is.null(mat)) return(FALSE)
    shape <- dim(mat)
    ret <- mapply(
        FUN = is.integer,
        x = mat,
        USE.NAMES = TRUE
    )
    ret <- matrix(ret, nrow = shape[1], dimnames = dimnames(mat))
    return(ret)
}


.is_integer_list <- function(lst)
{
    if (is.null(lst) | length(lst) == 0) return(FALSE)

    if ( is.null(dim(lst)) & inherits(lst, "list") )
    {
        ret <- lapply(
            X = lst,
            FUN = function(x)
            {
                if (is.null(x) | length(x) == 0) return(FALSE)
                .is_integer_list(x)
            }
        )
    }

    if (is.atomic(lst) & is.null(dim(lst)))
        ret <- .is_integer_vector(lst)

    if (inherits(lst, "matrix"))
        ret <- .is_integer_matrix(lst)

    if (inherits(lst, "data.frame"))
        ret <- .is_integer_dataframe(lst)

    return(ret)
}


.is_integer_dataframe <- function(df)
{
    ret <- vapply(
        X = df,
        FUN = .is_integer_vector,
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
    is_integer(test_list)
    is_integer(test_dataframe)
}
