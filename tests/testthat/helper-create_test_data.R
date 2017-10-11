

library(dplyr)
library(stringi)
library(lubridate)


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
    num      = 1:10 , 
    num_na   = c( 1:9 , NA) ,
    flt      = 0.5, 
    flt2     = 0.5 - 0.000000000001,
    flt_calc = 0.58 - 0.08,
    chr      = c('antelope', 'bear', 'cake', 'gpro/'),
    chr_na   = c('antelope', 'bear', 'cake', NA),
    chr_one  = 'duck',
    fct      = factor(c('apple', 'ball','2')),
    fct_na   = factor(c('apple', 'ball',NA)),
    lgl      = c(TRUE,FALSE,TRUE),
    lgl_na   = c(TRUE, FALSE, NA),
    null     = NULL,
    na       = NA  
)
