
context("Testing value differnces")



#function to make a test data set. 
returndata <- function(x,y){
    tibble(
        testvar.x = x, 
        testvar.y =y , 
        keys=seq(1,length(x))
    )
}


run_diff_test <- function( var1, var2 , expect_diff ){
    CALL  <- match.call()
    expect_equal( 
        identify_variable_diff('testvar', returndata(var1, var2), 'keys') %>% nrow,
        expect_diff,
        info = str_c("var1 = " , as.character(CALL[2]) , "\nvar2 = " , as.character(CALL[3])) ,
        label =  "identify_variable_diff returns a row count"
    )
}


test_that( "Number of differences are flagged accordingly",{
    run_diff_test( VALS$num     , VALS$num     , 0 )
    run_diff_test( VALS$num     , VALS$int     , 0 )
    run_diff_test( VALS$num_na  , VALS$num_na  , 0 )
    run_diff_test( VALS$flt     , VALS$flt     , 0 )
    run_diff_test( VALS$chr     , VALS$chr     , 0 )
    run_diff_test( VALS$chr_na  , VALS$chr_na  , 0 )
    run_diff_test( VALS$chr_one , VALS$chr_one , 0 )
    run_diff_test( VALS$fct     , VALS$fct     , 0 )
    run_diff_test( VALS$fct_na  , VALS$fct_na  , 0 )
    run_diff_test( VALS$lgl     , VALS$lgl     , 0 )
    run_diff_test( VALS$lgl_na  , VALS$lgl_na  , 0 )
    run_diff_test( VALS$na      , VALS$na      , 0 )
    
    
    run_diff_test( VALS$num     , VALS$num_na  , 1 )
    run_diff_test( VALS$flt     , VALS$num     , 5 )
    run_diff_test( VALS$chr     , VALS$chr_na  , 2 )
    run_diff_test( VALS$fct     , VALS$fct_na  , 1 )
    run_diff_test( VALS$lgl     , VALS$lgl_na  , 1 )
})








