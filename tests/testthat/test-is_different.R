# library(purrr)
# library(dplyr)
# library(testthat)
# source("./tests/testthat/helper-create_test_data.R")


test_that( "is_different correclty doesn't flag identical objects", {
    expect_self <- function(x , name)  expect_true( !is_different( x , x) %>%  all, label = name ) 
    
    walk2( VALS , names(VALS) , expect_self)
    
    expect_true( !is_different( VALS$flt , VALS$flt_calc) %>%  all )
})

test_that( "is_different correctly flags differences", {
    expect_equal( is_different( VALS$num, VALS$num_na) , c(rep(F,9),T) )
    expect_equal( is_different( VALS$flt, VALS$flt2)   , F )
    expect_equal( is_different( VALS$chr, VALS$chr_na) , c(rep(F,3),T) )
    expect_equal( is_different( VALS$fct, VALS$fct_na) , c(rep(F,2),T) )
    expect_equal( is_different( VALS$lgl, VALS$lgl_na) , c(rep(F,2),T) )
})
    
test_that( "is_different throws a warning if vectors are of a different length", {
    msg <- "Inputs are not of the same length"
    expect_warning(is_different( c(1,2,3) , c(1,2,NULL)), regexp =  msg)
    expect_warning(is_different( c(1,2,3) , c(1,2)), regexp =  msg)
})
