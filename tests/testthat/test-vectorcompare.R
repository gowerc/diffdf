library(purrr)
library(dplyr)
library(readr)
library(testthat)


x_num <- seq(1,10)
x_num_na <- x_num
x_num_na[10] <- NA
x_floating <-0.58-0.08
x_floating_comp <- 0.5
x_character <- c('antelope', 'bear', 'cake', 'gpro/')
x_character_na <-  c('antelope', 'bear', 'cake', NA)
x_character_one <- 'duck'
x_factor <- factor(c('apple', 'ball','2'))
x_factor_na <- factor(c('apple', 'ball',NA))
x_logical <- c(TRUE,FALSE,TRUE)
x_logical_na <- c(TRUE, FALSE, NA)
x_null <- NULL
x_na <-NA


#simple function to check equality between vectorcompare and a logical vector
eqtest <- function(test1, test2, compob){
  all.equal(vectorcompare(test1, test2), compob)
}


test_that( "Check comparision of equal objects (should be all false)",{
  
  expect_true( eqtest(x_num, x_num, rep(FALSE, 10)))
  expect_true( eqtest(x_num_na, x_num_na, rep(FALSE, 10)))
  expect_true( eqtest(x_floating, x_floating, rep(FALSE, 1)))
  expect_true( eqtest(x_floating, x_floating_comp, rep(FALSE, 1)))
  expect_true( eqtest(x_character, x_character, rep(FALSE, 4)))
  expect_true( eqtest(x_character_na, x_character_na, rep(FALSE, 4)))
  expect_true( eqtest(x_character_one, x_character_one, rep(FALSE, 1)))
  expect_true( eqtest(x_factor, x_factor, rep(FALSE, 3)))
  expect_true( eqtest(x_factor_na, x_factor_na, rep(FALSE, 3)))
  expect_true( eqtest(x_logical_na, x_logical_na, rep(FALSE, 3)))
  expect_true( eqtest(x_na, x_na, rep(FALSE, 1)))
})


test_that( "Check comparision of different objects",{
  
  expect_true( eqtest(x_num, x_num_na, c(rep(F,9), T)))
  expect_true( eqtest(x_floating, x_floating+0.01, T))
  expect_true( eqtest(x_character, x_character_na, c(rep(F,3), T)))
  expect_true( eqtest(x_factor, x_factor_na, c(rep(F,2), T)))
  expect_true( eqtest(x_logical, x_logical_na, c(rep(F,2), T)))
  
  
})