# RCOMPARE


## Introduction

Rcompare is a package for R which enables detailed comparison of data frames in R. Similar in conception to R, it provides a detailed level of comparisons between two data frames.

It is called via the library(rcompare), then using

rcompare(data1,data2).

You can save the outputted information into a file, and also interogate the objects produced in more detail.

## Features

The following is possible in rcompare

- Difference in number of columns/rows
- Comparison of attributes (such as labels, factor levels)
- Checking columns are of the same class and mode
- Controlling the level of accuracy used when comparing numeric values

Note that currently rcompare only supports data frames, and columns which are not lists.

## Installation

rcompare is not currently on gran, so you can install it via


devtools::install_git(“https://github.roche.com/gowerc/rcompare.git” , upgrade_dependencies = FALSE)
library(rcompare)
rcompare( iris,  iris[-5] ) 

Use ?rcompare for more help. For any issues, please use the issues tracker on github.
