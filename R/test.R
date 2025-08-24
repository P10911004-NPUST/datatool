test_vector <- function(fun)
{
    x <- list(
        int = c(1L, -6L, 54L, 92L, -773L),
        dbl = stats::rnorm(5) + stats::runif(5),
        cplx = complex(real = 1:5, imaginary = 3:7),
        inf = c(Inf, -Inf, Inf, Inf, -Inf),
        char = c("pds", "wskdg", "ddkgb", "sodgn", "a"),
        bool = c(TRUE, TRUE, FALSE, TRUE, FALSE),
        na = rep(NA, 5),
        nan = rep(NaN, 5),
        null = NULL,
        zero_int = integer(0),
        zero_char = character(0),
        zero_bool = logical(0)
    )

    return(fun(x))
}

test_list <- function(fun)
{
    x <- list(
        int = list(A = integer(3), B = list(B1 = integer(3)), C = integer(3)),
        dbl = list(A = stats::rnorm(3), B = list(B1 = stats::rnorm(3)), C = stats::rnorm(3)),
        cplx = list(A = complex(3), B = list(B1 = complex(3)), C = complex(3)),
        inf = list(A = c(Inf, -Inf), B = list(B1 = -Inf, Inf), C = c(Inf, Inf)),
        char = list(A = LETTERS[1:3], B = list(B1 = letters[1:3]), C = character(3)),
        bool = list(A = logical(3), B = list(B1 = c(TRUE, FALSE, TRUE)), C = logical(3)),
        na = list(NA_real = NA_real_, NA_int = NA_integer_, NA_cplx = NA_complex_, NA_char = NA_character_),
        nan = list(matrix(rep(NaN, 9), 3, dimnames = list(1:3, c("A", "B", "C")))),
        null = list(A = NULL, B = list(B1 = logical(0), B2 = character(0)), C = list(C1 = integer(0), C2 = complex(0)))
    )
    return(fun(x))
}


test_matrix <- function(fun)
{
    x <- list(
        int = list(A = matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4])),
                   NA_integer = matrix(rep(NA_integer_, 12), 4)),
        dbl = list(A = matrix(stats::rnorm(9), nrow = 3, dimnames = list(1:3, letters[1:3])),
                   NA_double = matrix(rep(NA_real_, 12), 4)),
        cplx = list(A = matrix(complex(9), 3)),
        inf = list(A = matrix(c(Inf, -Inf, -Inf, -Inf), 2)),
        char = list(A = matrix(LETTERS[1:12], 3), B = list(B1 = matrix(letters[1:9], 3))),
        bool = list(A = matrix(logical(9), 3), B = list(B1 = matrix(c(TRUE, FALSE, TRUE, TRUE), 2))),
        na = list(NA_bool = matrix(rep(NA, 9), 3),
                  NA_cplx = list(B1 = matrix(rep(NA_complex_, 12), 4)),
                  NA_char = matrix(rep(NA_character_, 9), 3)),
        nan = list(A = matrix(rep(NaN, 9), 3),
                   B = list(matrix(rep(NaN, 12), 4, dimnames = list(1:4, LETTERS[1:3])))),
        null = list(null = NULL,
                    zero_bool = matrix(logical(0)),
                    zero_char = matrix(character(0)),
                    zero_int = matrix(integer(0)),
                    zero_dbl = matrix(double(0)),
                    zero_cplx = matrix(complex(0)))
    )
    return(fun(x))
}

test_dataframe <- function(fun)
{
    x <- list(
        A = data.frame(),
        B = data.frame(
            dbl = double(5),
            int = integer(5),
            cplx = complex(5),
            char = LETTERS[1:5],
            bool = logical(5),
            inf = c(Inf, -Inf, Inf, Inf, -Inf),
            special = c(NaN, NA, NA, NaN, NA)
        )
    )
    return(fun(x))
}


