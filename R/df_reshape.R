#' @title
#' Reshape Data Frame
#'
#' @description
#' Reshape a data frame from wide format to long format.
#'
#' @param data A data.frame input
#' @param cols A character/numeric vector of column names or indices to select the columns.
#' @param keys_name A new column name for the `cols`.
#' @param vals_name A new column name for the values from the `cols`.
#' @param keep_unused Logical (default: TRUE). Keep the other columns that are not included in `cols`.
#'
#' @return A reshaped data.frame
#' @export
#'
#' @examples
#' set.seed(1)
#' df0 <- as.data.frame(matrix(1:25, ncol = 5, dimnames = list(1:5, LETTERS[1:5])))
#' wide_to_long(df0, cols = c("A", "B", "D"), keys_name = "group", vals_name = "length")
#' wide_to_long(df0, cols = c(2, 3, 5), keys_name = "group", vals_name = "length", keep_unused = FALSE)
wide_to_long <- function(
        data,
        cols,
        keys_name = character(1),
        vals_name = character(1),
        keep_unused = TRUE
) {
    if ( ! inherits(data, "data.frame") ) stop("Input data should be a data.frame")

    df0 <- data

    if (is.character(cols))
    {
        col_names <- cols
        col_indices <- match(cols, colnames(data))
    }

    if (is.numeric(cols))
    {
        col_names <- colnames(data)[cols]
        col_indices <- cols
    }

    df0 <- stats::reshape(
        data = df0,
        direction = "long",
        varying = col_indices,
        v.names = vals_name,
        times = col_names,
        timevar = keys_name
    )

    df0 <- df0[ 1 : (ncol(df0) - 1) ]
    row.names(df0) <- NULL

    if ( ! keep_unused ) df0 <- df0[, c(keys_name, vals_name), drop = FALSE]

    return(df0)

    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Testing
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    if (FALSE)
    {
        set.seed(1)
        df0 <- as.data.frame(matrix(1:25, ncol = 5, dimnames = list(1:5, LETTERS[1:5])))

        wide_to_long(
            df0,
            cols = c("A", "B", "D"),
            keys_name = "group",
            vals_name = "length"
        )

        wide_to_long(
            df0,
            cols = c(2, 3, 5),
            keys_name = "group",
            vals_name = "length",
            keep_unused = FALSE
        )
    }

}

#
# long_to_wide <- function(
#         data,
#         cols,
#         val,
#         keep_unused = TRUE
# ) {
#
#     if (inherits(data, "data.frame")) mat <- as.matrix(data)
# }
#
# data("iris")
# df <- iris
# reshape(df, direction = "wide",
#         idvar = c("Petal.Width", "Petal.Length", "Sepal.Width"),  # exclude the `val` column
#         timevar = "Species", # Which column as key
#         sep = ""
# )

