
context("Testing find_difference")

test_that( "find_difference correctly doesn't flag identical objects", {
    
    expect_self <- function(x , name)  {
        expect_true( !find_difference( x , x) %>%  all, label = name ) 
    }
    
    purrr::walk2( VALS , names(VALS) , expect_self)
    
    expect_true( !find_difference( VALS$flt , VALS$flt_calc) %>%  all )
})



test_that( "find_difference correctly flags differences", {
    expect_equal( find_difference( VALS$num, VALS$num_na) , c(F,F,F,F,T))
    expect_equal( find_difference( VALS$flt, VALS$flt2)   , c(F,F,F,F,F))
    expect_equal( find_difference( VALS$chr, VALS$chr_na) , c(F,F,F,T,T))
    expect_equal( find_difference( VALS$fct, VALS$fct_na) , c(F,F,T,F,F))
    expect_equal( find_difference( VALS$lgl, VALS$lgl_na) , c(F,F,T,F,F))
})

test_that("find_difference correctly uses tolerances/scale arguments",{
    expect_equal( find_difference( VALS$flt, VALS$flt2) , c(F,F,F,F,F))
    expect_equal( find_difference( VALS$flt, VALS$flt2, tolerance = 0.0000000000001 ) , c(T,T,T,T,T)) 
    expect_equal( find_difference( VALS$flt, VALS$flt3) , c(F,F,F,F,F)) 
    expect_equal( find_difference( VALS$flt, VALS$flt3, tolerance = 0.0000000000001 ) , c(T,F,F,F,F)) 
    expect_equal( find_difference( VALS$flt, VALS$flt2, tolerance = 1,
                                scale = 1e-13) , c(T,T,T,T,T)) 
    expect_equal( find_difference( VALS$flt, VALS$flt_calc) , c(F,F,F,F,F)) 
    expect_equal( find_difference( VALS$flt, VALS$flt_calc, tolerance = 1e-17) , VALS$flt!=VALS$flt_calc) 
})
    



test_that( "find_difference throws a warning if vectors are of a different length", {
    msg <- "Inputs are not of the same length"
    expect_warning(find_difference( c(1,2,3) , c(1,2,NULL)), regexp =  msg)
    expect_warning(find_difference( c(1,2,3) , c(1,2)), regexp =  msg)
})
