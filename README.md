
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matador

<!-- badges: start -->
<!-- badges: end -->

matador is a simple package intended for plotting and working with
matrices. It is intended to help students complete typical linear
algebra problems using Rmarkdown.

## Installation

matador depends on several of the core tidyverse packages, namely purrr,
dplyr, and ggplot2, as well as the ggplot2 extension cowplot.

## Examples

Here are a few demonstrations of matador’s functionality:

matador makes it easy to visualize two-dimensional linear
transformations:

``` r
library(matador)
#> Loading required package: magrittr

plot_transform(trans = matrix(c(-1, 1,1, -1), nrow =2))
#> Scale for 'linetype' is already present. Adding another scale for 'linetype', which will replace
#> the existing scale.
#> Scale for 'linetype' is already present. Adding another scale for 'linetype', which will replace
#> the existing scale.
```

<img src="man/figures/README-example-1.png" width="100%" />

You can do the same for systems of equations:

``` r
plot_lines(m = matrix(c(-5, 2, 3, 7), nrow =2))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Or easily compute arbitrary powers of matrices:

``` r
mat_pows(matrix(1:9, nrow = 3), 1:5)
#> $`1`
#>      [,1] [,2] [,3]
#> [1,]    1    4    7
#> [2,]    2    5    8
#> [3,]    3    6    9
#> 
#> $`2`
#>      [,1] [,2] [,3]
#> [1,]   30   66  102
#> [2,]   36   81  126
#> [3,]   42   96  150
#> 
#> $`3`
#>      [,1] [,2] [,3]
#> [1,]  468 1062 1656
#> [2,]  576 1305 2034
#> [3,]  684 1548 2412
#> 
#> $`4`
#>       [,1]  [,2]  [,3]
#> [1,]  7560 17118 26676
#> [2,]  9288 21033 32778
#> [3,] 11016 24948 38880
#> 
#> $`5`
#>        [,1]   [,2]   [,3]
#> [1,] 121824 275886 429948
#> [2,] 149688 338985 528282
#> [3,] 177552 402084 626616
```

For more details, take a look at the package vignette.

## Acknowledgments

This package could not have been developed without the invaluable
[devtools](https://cran.r-project.org/web/packages/devtools/index.html)
package, which simplifies and automates the process of package
development.

The make\_axes function benefited greatly from
[this](https://stackoverflow.com/questions/17753101/center-x-and-y-axis-with-ggplot2)
StackOverflow exchange.
