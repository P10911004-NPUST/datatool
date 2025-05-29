`_is_number` <- function(
        `_x` = double(1),
        `_include_boolean` = FALSE,
        `_include_complex` = TRUE,
        `_include_infinite` = FALSE,
        `_include_nan` = FALSE
) {
    ret_bool <- is.numeric(`_x`)

    if ( `_include_boolean` ) ret_bool <- ret_bool | isTRUE(`_x`) | isFALSE(`_x`)
    if ( `_include_complex` ) ret_bool <- ret_bool | is.complex(`_x`)
    if ( ! `_include_infinite` ) ret_bool <- ret_bool & is.finite(`_x`)
    if ( ! `_include_nan` ) ret_bool <- ret_bool & !is.nan(`_x`)

    return(ret_bool)

    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Testing
    x <- list(1, 2i, NA_character_, NA_complex_, NA_integer_, NA_real_, -3, Inf, -Inf, NA, NaN)
    for (i in x){
        print(sprintf("%s : %s", i, `_is_number`(i)))
    }
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
}


