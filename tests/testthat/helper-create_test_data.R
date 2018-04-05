
library(testthat)
library(rcompare)
library(purrr)
library(tibble)

set.seed(20202)

LENGTH <- 20 

TDAT <- dplyr::data_frame( 
    ID          = 1:LENGTH,
    GROUP1      = rep( c(1,2) , each = LENGTH/2),
    GROUP2      = rep( c(1:(LENGTH/2)), 2 ),
    INTEGER     = rpois(LENGTH , 40),
    BINARY      = sample( c("M" , "F") , LENGTH , replace = T),
    DATE        = lubridate::ymd("2000-01-01") + rnorm(LENGTH, 0, 7000),
    DATETIME    = lubridate::ymd_hms("2000-01-01 00:00:00") + rnorm(LENGTH, 0, 200000000), 
    CONTINUOUS  = rnorm(LENGTH , 30 , 12),
    CATEGORICAL = factor(sample( c("A" , "B" , "C") , LENGTH , replace = T)),
    LOGICAL     = sample( c(TRUE , FALSE) , LENGTH , replace = T),
    CHARACTER   = stringi::stri_rand_strings(LENGTH,  rpois(LENGTH , 13),  pattern = "[ A-Za-z0-9]")
)


VALS <- list(
    int      = 1:5 , 
    int_na   = c( 1:5 , NA) ,
    num      = c(1,2,3,4,5),
    num_na   = c(1,2,3,4,NA), 
    flt      = c(0, 0.1, 0.2 , 0.3, 0.4) ,
    flt2     = c(0, 0.1, 0.2 , 0.3, 0.4) - 0.000000000001,
    flt_calc = c(0.08, 0.18, 0.28 , 0.38, 0.48) - 0.08,
    chr      = c('antelope', 'bear', 'cake', 'gpro/', "^admw"),
    chr_na   = c('antelope', 'bear', 'cake', NA , "@awd"),
    chr_one  = 'duck',
    fct      = factor(c('apple', 'ball','2' , T , "pears")),
    fct_na   = factor(c('apple', 'ball',NA, T , "pears")),
    lgl      = c(TRUE,FALSE,TRUE, T , F),
    lgl_na   = c(TRUE, FALSE, NA, T , F),
    null     = NULL,
    na       = NA  
)



### Setup test data
TDAT2 <- TDAT

## Unequal values
TDAT2$CONTINUOUS[c(1,5,7)] <- c( 1,2,3)

## Different attributes
attr(TDAT2$BINARY , "something") <- iris

## Different levels
levels(TDAT2$CATEGORICAL) <- c("A", "B" , "D")

## Different class
class( TDAT2$DATE) <-  c("A_DATE" , "b_date" , "cDate")

## Different mode
TDAT2$INTEGER[c(1,5,7)] <- c("1" , "2" , "3")

## Large number of differences
TDAT2$GROUP2 <- rpois(20, 50)

attr(TDAT2$DATETIME , "label") <- "This is the label for my amazing variable"
