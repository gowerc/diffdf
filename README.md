
# diffdf

<!-- start badges -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/diffdf)](https://CRAN.R-project.org/package=diffdf)
[![Dependencies](https://tinyverse.netlify.com/badge/diffdf)](https://cran.r-project.org/package=diffdf)
[![Downloads](https://cranlogs.r-pkg.org/badges/diffdf?color=brightgreen)](https://www.r-pkg.org/pkg/diffdf)
[![Code Coverage 📔](https://raw.githubusercontent.com/gowerc/diffdf/_xml_coverage_reports/data/master/badge.svg)](https://gowerc.github.io/diffdf/main/coverage-report/)
<!-- end badges -->

The diffdf package is designed to enable detailed comparison of two data.frames. Whilst many packages exist for informing you if there are differences between data.frames, none provide as much detail on what and where those differences are as diffdf does!

Currently diffdf supports the following:

- Checking for differences in values
- Checking for differences in attributes
- Checking for differences in classes
- Checking for differences in column names
- Checking for differences in the number of observations
- Matching rows by key/id variables
- Fuzzy comparisons (i.e. treating doubles and integers as the same)
- Extracting datasets of different rows

For more information on features please consult the vignette and man pages.

## Installation

You can install the released version of diffdf from [CRAN](https://CRAN.R-project.org/package=diffdf) with:

``` r
install.packages("diffdf")
```

And the development version from [GitHub](https://github.com/gowerc/diffdf) with:

``` r
# install.packages("devtools")
devtools::install_github("gowerc/diffdf")
```

## Example

``` r
library(diffdf)
iris2 <- iris
for (i in 1:3) iris2[i,i] <- i^2
iris2$new_var <- "hello"
class(iris2$Species) <- "some class"
diffdf(iris, iris2)
```
