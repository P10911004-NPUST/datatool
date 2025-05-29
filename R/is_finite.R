# Internal function
`_is_finite` <- function(
        `_x` = double(1),
        `_include_boolean` = FALSE,
        `_include_complex` = TRUE
) {
    if (is.null(`_x`)) return(NULL)

    # is.numeric() will exclude boolean and complex value
    ret_bool <- is.numeric(`_x`) & is.finite(`_x`)

    if (`_include_boolean`) ret_bool <- ret_bool | isTRUE(`_x`) | isFALSE(`_x`)
    if (`_include_complex`) ret_bool <- ret_bool | (is.complex(`_x`) & !is.na(`_x`))

    return(ret_bool)
}


is_finite <- function(
        x = double(),
        include_boolean = FALSE,
        include_complex = TRUE
) {
    if (is_vector(x)) return(`_is_finite`(x, include_boolean, include_complex))
    if (is_matrix(x)) return(`_is_finite`(x, include_boolean, include_complex))
    if (is_array(x)) return(`_is_finite`(x, include_boolean, include_complex))

    if (is_list(x))
    {
        return(
            lapply(
                X = x,
                FUN = function(x) `_is_finite`(x, include_boolean, include_complex)
            )
        )
    }


    if (is_dataframe(x))
    {
        return(
            vapply(
                X = x,
                FUN = function(x) `_is_finite`(x, include_boolean, include_complex),
                FUN.VALUE = logical(nrow(x))
            )
        )
    }

    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Testing
    lst <- list(
        num = c(1, 2.5, -8, 3e5, NA_real_),
        comp = c(2i, 3i, NA_complex_),
        bool = c(TRUE, FALSE),
        chr = LETTERS[1:3],
        inf = c(Inf, -Inf),
        special = c(NULL, NaN)
    )
    is_finite(lst)

    df <- data.frame(
        num = c(1, 2.5, -8, 3e5, NA_real_),
        comp = c(2i, 3i, 4i, NA_complex_, NA_complex_),
        bool = c(TRUE, FALSE, TRUE, TRUE, FALSE),
        chr = LETTERS[1:5],
        inf = c(Inf, -Inf, Inf, Inf, -Inf),
        nan = rep(NaN, times = 5)
    )
    is_finite(df)

    mat <- list(
        num = matrix(c(1, 2.5, -8, 3e5, NA_real_, NA_real_), nrow = 2),
        comp = matrix(c(1i, 2i, 3i, 4i, NA_complex_, NA_complex_), nrow = 2),
        bool = matrix(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE), nrow = 2),
        chr = matrix(LETTERS[1:6], nrow = 2),
        inf = matrix(c(Inf, -Inf, Inf, Inf, -Inf, Inf), nrow = 2),
        nan = matrix(rep(NaN, times = 6), nrow = 2)
    )
    is_finite(mat)
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
}



