`_is_number` <- function(
        x,
        `_include_infinite` = FALSE,
        `_include_complex` = FALSE,
        `_include_nan` = FALSE
) {
    ret_bool <- is.numeric(x)

    if ( ! `_include_infinite` )
        ret_bool[is.infinite(ret_bool)] <- FALSE

    if (`_include_complex`)
        ret_bool[is.complex(ret_bool)] <- TRUE

    return(ret_bool)

    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Testing
    x <- list(1, 2i, NA_character_, NA_complex_, NA_integer_, NA_real_, -3, Inf, -Inf, NA, NaN)
    for (i in x){
        print(sprintf("%s : %s", i, `_is_number`(i)))
    }
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
}

is.infinite(Inf)


