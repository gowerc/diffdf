# diffdf

Find data.frame differences !

## Introduction

The diffdf package is designed to enable detailed comparisons of data.frames. Whilst many packages exist for informing you if there are differences between data.frames non provide as much detail on what and where those differences are as diffdf !

## Basic example

```
library(diffdf)
iris2 <- iris
for ( i in 1:3) iris2[i,i] <- i^2
iris2$new_var <- "hello"
class(iris2$Species) <- "some class"
dfdiff( iris, iris2)
```

For more information on features please consult the vignette and man pages. 



