test_vector <- list(
    int = c(1L, -6L, 54L, 92L, -773L),
    dbl = rnorm(5) + runif(5),
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

test_list <- list(
    int = list(A = integer(3), B = list(B1 = integer(3)), C = integer(3)),
    dbl = list(A = rnorm(3), B = list(B1 = rnorm(3)), C = rnorm(3)),
    cplx = list(A = complex(3), B = list(B1 = complex(3)), C = complex(3)),
    inf = list(A = c(Inf, -Inf), B = list(B1 = -Inf, Inf), C = c(Inf, Inf)),
    char = list(A = LETTERS[1:3], B = list(B1 = letters[1:3]), C = character(3)),
    bool = list(A = logical(3), B = list(B1 = c(TRUE, FALSE, TRUE)), C = logical(3)),
    na = list(
        NA_real = NA_real_,
        B = list(
            NA_cplx = NA_complex_,
            Na_int = NA_integer_),
        NA_char = NA_character_),
    nan = list(A = rep(NaN, 3), B = list(B1 = rep(NaN, 3), B2 = NaN), C = NaN),
    null = list(A = NULL, B = list(B1 = NULL, B2 = NULL), C = list(NULL, NULL))
)

test_matrix <- list(
    int = list(A = matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4])),
               B = matrix(integer(12), 4)),
    dbl = list(A = matrix(rnorm(9), nrow = 3, dimnames = list(1:3, letters[1:3]))),
    cplx = list(A = matrix(complex(9), 3)),
    inf = list(A = matrix(c(Inf, -Inf, -Inf, -Inf), 2)),
    char = list(A = matrix(LETTERS[1:12], 3), B = list(B1 = matrix(letters[1:9], 3))),
    bool = list(A = matrix(logical(9), 3), B = list(B1 = matrix(c(TRUE, FALSE, TRUE, TRUE), 2))),
    na = list(A = matrix(rep(NA_real_, 9), 3),
              B = list(B1 = matrix(rep(NA_complex_, 12), 4)),
              C = matrix(NA_character_)),
    nan = list(A = matrix(rep(NaN, 9), 3),
               B = list(B1 = matrix(rep(NaN, 12), 4))),
    null = list(A = NULL, B = matrix(logical(0)))
)

test_dataframe <- list(
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




