#' @title Identify Finite Numbers
#'
#' @description
#' A wrapper for `base::is.finite()`. By default, `statool::is_finite()` doesn't
#' consider logical value (i.e., `TRUE` and `FALSE`) as finite values,
#' which is differ from `base::is.finite()`.
#'
#' @param x An atomic or recursive vector (i.e., c(), matrix(), list(), or data.frame()).
#' @param include_boolean Logical (default: FALSE). Consider boolean values as finite?
#' @param include_complex Logical (default: TRUE). Consider complex values as finite?
#'
#' @return A logical vector which has the same length / dimensions with `x`.
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
#'     special = list(TRUE, FALSE, NaN, NULL, Inf, -Inf)
#' )
#' is_finite(test)
is_finite <- function(
        x,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    UseMethod("is_finite")
}

#' @export
is_finite.default <- function(
        x,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    .is_finite_vector(x, include_boolean, include_complex)
}

#' @export
is_finite.matrix <- function(
        x,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    .is_finite_matrix(x, include_boolean, include_complex)
}

#' @export
is_finite.list <- function(
        x,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    .is_finite_list(x, include_boolean, include_complex)
}

#' @export
is_finite.data.frame <- function(
        x,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    .is_finite_dataframe(x, include_boolean, include_complex)
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Internal functions ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
.is_finite_vector <- function(
        vct,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    if ( is.null(vct) | length(vct) == 0 ) return(FALSE)

    vapply(
        X = vct,
        FUN = function(x, .include_boolean, .include_complex)
        {
            ret <- is.finite(x)
            if ( ! .include_boolean ) ret <- ret && !(isTRUE(x) || isFALSE(x))
            if ( ! .include_complex ) ret <- ret && !is.complex(x)
            return(ret)
        },
        .include_boolean = include_boolean,
        .include_complex = include_complex,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
    )
}


.is_finite_matrix <- function(
        mat,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    if ( is.null(mat) | length(mat) == 0 ) return(FALSE)
    if (is.null(mat)) return(FALSE)

    shape <- dim(mat)
    ret <- mapply(
        FUN = function(x, .include_boolean, .include_complex)
        {
            ret <- is.finite(x)
            if ( ! .include_boolean ) ret <- ret && !(isTRUE(x) || isFALSE(x))
            if ( ! .include_complex ) ret <- ret && !is.complex(x)
            return(ret)
        },
        .include_boolean = include_boolean,
        .include_complex = include_complex,
        x = mat
    )
    ret <- matrix(ret, nrow = shape[1], dimnames = dimnames(mat))
    return(ret)
}


.is_finite_list <- function(
        lst,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    if (is.null(lst) | length(lst) == 0) return(FALSE)

    if ( is.null(dim(lst)) & inherits(lst, "list") )
    {
        ret <- lapply(
            X = lst,
            FUN = function(x, .include_boolean, .include_complex)
            {
                if (is.null(x) | length(x) == 0) return(FALSE)
                .is_finite_list(x, .include_boolean, .include_complex)
            },
            .include_boolean = include_boolean,
            .include_complex = include_complex
        )
    }

    if (is.atomic(lst) & is.null(dim(lst)))
        ret <- .is_finite_vector(lst, include_boolean, include_complex)

    if (inherits(lst, "matrix"))
        ret <- .is_finite_matrix(lst, include_boolean, include_complex)

    if (inherits(lst, "data.frame"))
        ret <- .is_finite_dataframe(lst, include_boolean, include_complex)

    return(ret)
}


.is_finite_dataframe <- function(
        df,
        include_boolean = FALSE,
        include_complex = TRUE
) {
    ret <- vapply(
        X = df,
        FUN = function(x, .include_boolean, .include_complex)
        {
            .is_finite_vector(x, .include_boolean, .include_complex)
        },
        .include_boolean = include_boolean,
        .include_complex = include_complex,
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
    is_finite(test_vector)
    is_finite(test_matrix, include_boolean = TRUE, include_complex = FALSE)
    is_finite(test_list)
    is_finite(test_dataframe)
}
