#' @title
#' Identify Character Values
#'
#' @description
#' A wrapper for the `base::is.character()`.
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
#' is_character(test)
is_character <- function(x)
{
    UseMethod("is_character")
}
#' @export
is_character.default <- function(x) .is_character_vector(x)
#' @export
is_character.matrix <- function(x) .is_character_matrix(x)
#' @export
is_character.list <- function(x) .is_character_list(x)
#' @export
is_character.data.frame <- function(x) .is_character_dataframe(x)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Internal functions ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
.is_character_vector <- function(vct)
{
    if ( is.null(vct) | length(vct) == 0 )
        return(FALSE)

    vapply(
        X = vct,
        FUN = is.character,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
    )
}


.is_character_matrix <- function(mat)
{
    if ( is.null(mat) | length(mat) == 0 )
        return(FALSE)

    shape <- dim(mat)
    ret <- mapply(
        FUN = is.character,
        x = mat,
        USE.NAMES = TRUE
    )
    ret <- matrix(ret, nrow = shape[1], dimnames = dimnames(mat))
    return(ret)
}


.is_character_list <- function(lst)
{
    if (is.null(lst) | length(lst) == 0)
        return(FALSE)

    if (is.null(dim(lst)) & inherits(lst, "list"))
    {
        ret <- lapply(
            X = lst,
            FUN = function(x)
            {
                if (is.null(x) | length(x) == 0) return(FALSE)
                .is_character_list(x)
            }
        )
    }

    if (is.atomic(lst) & is.null(dim(lst)))
        ret <- .is_character_vector(lst)

    if (inherits(lst, "matrix"))
        ret <- .is_character_matrix(lst)

    if (inherits(lst, "data.frame"))
        ret <- .is_character_dataframe(lst)

    return(ret)
}


.is_character_dataframe <- function(df)
{
    ret <- vapply(
        X = df,
        FUN = .is_character_vector,
        FUN.VALUE = logical(nrow(df)),
        USE.NAMES = TRUE
    )
    return(as.data.frame(ret))
}



