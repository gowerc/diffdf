
# diffdf

<!-- start badges -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/diffdf)](https://CRAN.R-project.org/package=diffdf)
[![Downloads](https://cranlogs.r-pkg.org/badges/diffdf?color=brightgreen)](https://www.r-pkg.org/pkg/diffdf)
[![Code Coverage 📔](https://raw.githubusercontent.com/gowerc/diffdf/_xml_coverage_reports/data/master/badge.svg)](https://gowerc.github.io/diffdf/master/coverage-report/)
<!-- end badges -->

`diffdf` compares two `data.frame` objects and provides a detailed summary of any differences that were found between them. The package has it's origins in supporting QC workflows for product development within the pharmaceutical industry aiming at being a light weight alternative to SAS's `PROC COMPARE`.

Currently diffdf supports the following:

- Checking for differences in:
  - Values
  - Attributes
  - Classes
  - Column names
  - Number of observations
  - Column ordering
- Matching rows by key/id variables
- Fuzzy comparisons (i.e. treating doubles and integers as the same)
- Extracting datasets of different rows

For more information on features please consult the vignette and man pages.

## Alternatives

If `diffdf` isn't quite right for your use case then the following are other packages that provide similar functionality that may be more appropriate:

- [`daff`](https://CRAN.R-project.org/package=daff)
- [`compareDF`](https://CRAN.R-project.org/package=compareDF)
- [`waldo`](https://CRAN.R-project.org/package=waldo)
- [`diffobj`](https://CRAN.R-project.org/package=diffobj)
- [`arsenal`](https://CRAN.R-project.org/package=arsenal)

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
