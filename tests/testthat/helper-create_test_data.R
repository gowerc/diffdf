

library(dplyr)
library(stringi)
library(lubridate)


set.seed(20202)

LENGTH <- 20 

test_data_core <- data_frame( 
  ID          = 1:LENGTH,
  GROUP1      = rep( c(1,2) , each = LENGTH/2),
  GROUP2      = rep( c(1:(LENGTH/2)), 2 ),
  INTEGER     = rpois(LENGTH , 40),
  BINARY      = sample( c("M" , "F") , LENGTH , replace = T),
  DATE        = ymd("2000-01-01") + rnorm(LENGTH, 0, 7000),
  DATETIME    = ymd_hms("2000-01-01 00:00:00") + rnorm(LENGTH, 0, 200000000), 
  CONTINUOUS  = rnorm(LENGTH , 30 , 12),
  CATEGORICAL = sample( c("A" , "B" , "C") , LENGTH , replace = T),
  LOGICAL     = sample( c(TRUE , FALSE) , LENGTH , replace = T),
  CHARACTER   = stri_rand_strings(LENGTH,  rpois(LENGTH , 13),  pattern = "[ A-Za-z0-9]")
)
