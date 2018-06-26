# diffdf

Find data.frame differences ! This package is now on CRAN, see https://cran.r-project.org/web/packages/diffdf/index.html . Master branch is always in line with the CRAN release, see devel branch for latest development version.

## Introduction

The diffdf package is designed to enable detailed comparison of two data.frames. Whilst many packages exist for informing you if there are differences between data.frames, none provide as much detail on what and where those differences are as diffdf does!

## Example

```
library(diffdf)
iris2 <- iris
for ( i in 1:3) iris2[i,i] <- i^2
iris2$new_var <- "hello"
class(iris2$Species) <- "some class"
diffdf( iris, iris2)
```

## Features

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



