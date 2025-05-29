is_vector <- function(x)
{
    is.null(dim(x)) & is.atomic(x)
}


is_matrix <- function(x)
{
    !is.null(dim(x)) & is.atomic(x) & inherits(x, "matrix")
}


is_array <- function(x)
{
    !is.null(dim(x)) & is.atomic(x) & inherits(x, "array")
}


is_list <- function(x)
{
    is.null(dim(x)) & is.recursive(x) & inherits(x, "list")
}


is_dataframe <- function(x)
{
    !is.null(dim(x)) & is.recursive(x) & inherits(x, "data.frame")
}

