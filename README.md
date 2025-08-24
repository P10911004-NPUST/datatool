# datatool
A collection of customized datatype-related basic functions. 

**The default behaviour of the functions are quite different from the `base` one.**
Please be cautious when using them.

### Summary
- Each function always return values (mostly boolean), instead of returning `NULL`, zero-length vector, or anything that is somewhat confusing and counter-intuitive;
- The diagnostic is very specific ( exp. `NA` is not considered as logical and `NaN` is not double type );
- The length and/or dimensions of the output are identical with the input ( except zero-length input ).

## Installation

You can install the development version of `datatool` from [GitHub](https://github.com/P10911004-NPUST/datatool/) with:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("P10911004-NPUST/datatool")
```

