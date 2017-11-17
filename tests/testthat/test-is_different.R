
context("Testing is_different")

test_that( "is_different correctly doesn't flag identical objects", {
    
    expect_self <- function(x , name)  {
        expect_true( !is_different( x , x) %>%  all, label = name ) 
    }
    
    walk2( VALS , names(VALS) , expect_self)
    
    expect_true( !is_different( VALS$flt , VALS$flt_calc) %>%  all )
})



test_that( "is_different correctly flags differences", {
    expect_equal( is_different( VALS$num, VALS$num_na) , c(F,F,F,F,T))
    expect_equal( is_different( VALS$flt, VALS$flt2)   , c(F,F,F,F,F))
    expect_equal( is_different( VALS$chr, VALS$chr_na) , c(F,F,F,T,T))
    expect_equal( is_different( VALS$fct, VALS$fct_na) , c(F,F,T,F,F))
    expect_equal( is_different( VALS$lgl, VALS$lgl_na) , c(F,F,T,F,F))
})

test_that("is_different correctly uses tolerances/scale arguments",{
    expect_equal( is_different( VALS$flt, VALS$flt2) , c(F,F,F,F,F))
    expect_equal( is_different( VALS$flt, VALS$flt2, tolerance = 0.000000000001 ) , c(T,T,T,T,T)) 
    expect_equal( is_different( VALS$flt, VALS$flt2, tolerance = 1,
                                scale = 1e-13) , c(T,T,T,T,T)) 
    expect_equal( is_different( VALS$flt, VALS$flt_calc) , c(F,F,F,F,F)) 
    expect_equal( is_different( VALS$flt, VALS$flt_calc, tolerance = 1e-15) , VALS$flt!=VALS$flt_calc) 
})
    



test_that( "is_different throws a warning if vectors are of a different length", {
    msg <- "Inputs are not of the same length"
    expect_warning(is_different( c(1,2,3) , c(1,2,NULL)), regexp =  msg)
    expect_warning(is_different( c(1,2,3) , c(1,2)), regexp =  msg)
})
