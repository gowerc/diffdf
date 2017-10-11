# library(purrr)
# library(dplyr)
# library(testthat)
# source("./tests/testthat/helper-create_test_data.R")

#function to make a test data set. 

returndata <- function(x,y){
    tibble(
        testvar.x = x, 
        testvar.y =y , 
        keys=seq(1,length(x))
    )
}


test_that( "Check comparision of equal objects",{
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$num, VALS$num), 'keys') %>% nrow,
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$num_na, VALS$num_na), 'keys') %>% nrow,
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$flt, VALS$flt), 'keys') %>% nrow, 
        0
    )
    
    expect_equal(
        identify_variable_diff('testvar', returndata(VALS$chr, VALS$chr), 'keys') %>% nrow,
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$chr_na, VALS$chr_na), 'keys') %>% nrow,
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$chr_one, VALS$chr_one), 'keys') %>% nrow, 
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$fct, VALS$fct), 'keys') %>% nrow, 
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$fct_na, VALS$fct_na), 'keys') %>% nrow, 
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$lgl, VALS$lgl), 'keys') %>% nrow,
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$lgl_na, VALS$lgl_na), 'keys') %>% nrow, 
        0
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$na, VALS$na), 'keys') %>% nrow,
        0
    )
})




test_that( "Check comparision of different objects",{
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$num, VALS$num_na), 'keys') %>% nrow,
        1
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$flt, VALS$num), 'keys') %>% nrow,
        10
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$chr, VALS$chr_na), 'keys') %>% nrow,
        1
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$fct, VALS$fct_na), 'keys') %>% nrow, 
        1
    )
    
    expect_equal( 
        identify_variable_diff('testvar', returndata(VALS$lgl, VALS$lgl_na), 'keys') %>% nrow,
        1
    )
    
})



