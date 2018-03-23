

context("Testing mode difference functions")

run_mode_test <- function( dsin1, dsin2 , expect_diff ){
    CALL  <- match.call()
    expect_equal( 
        identify_mode_differences(dsin1, dsin2) %>% nrow,
        expect_diff,
        info = str_c("dsin1 = " , as.character(CALL[2]) , "\ndsin2 = " , as.character(CALL[3])) ,
        label =  "identify_mode_differences returns a row count"
    )
}


#' run_mode_test_with_N_obs
#' 
#' Convenience function which allows a user to determine the length of an
#' object then perform tests on it
#' @param N length of vectors being tested
#' @return nothing if no failure, otherwise error messages will be printed
#' 
#' 
run_mode_test_with_N_obs <-function(N){
    
    
    ###### Setup test data of different modes ----
    #
    #
    
    X_1col_int <- data_frame(
        x = seq(1, N)
    )
    
    X_1col_num <- data_frame(
        x = rnorm(N)
    )
    
    X_1col_num2 <- data_frame(
        x = round(rnorm(N))
    )
    
    X_1col_char <- data_frame(
        x = letters[1:N]
    )
    
    X_1col_char2 <- data_frame(
        x = rep('cat', N)
    )
    
    X_1col_fact <- data_frame(
        x = factor(letters[1:N])
    )
    
    X_1col_fact2 <- data_frame(
        x = factor(rep(c('cat', 'dog'), c(floor(N/2), ceiling(N/2))))
    )
    
    X_1col_lgl <- data_frame(
        x = rep(T, N)
    )
    
    X_1col_lgl2 <- data_frame(
        x = rep(c(T,F), c(floor(N/2), ceiling(N/2)))
    )
    
    X_2col_numint <- data_frame(
        x = X_1col_num$x,
        y = X_1col_int$x
    )
    
    X_2col_charint <- data_frame(
        x = X_1col_char$x,
        y = X_1col_int$x
    )
    
    X_2col_intchar <- data_frame(
        x = X_1col_int$x,
        y = X_1col_char$x
    )
    
    X_2col_numchar <- data_frame(
        x = X_1col_num$x,
        y = X_1col_char$x
    )
    
    X_2col_numnum <- data_frame(
        x = X_1col_num$x, 
        y = X_1col_num2$x
    )
    
    X_2col_numlgl <- data_frame(
        x = X_1col_num$x, 
        y = X_1col_lgl$x
    )
    
    X_2col_numfact <- data_frame(
        x = X_1col_num$x, 
        y = X_1col_fact$x
    )
    
    X_2col_charlgl <- data_frame(
        x = X_1col_char$x, 
        y = X_1col_lgl$x
    )
    
    X_2col_charfact <- data_frame(
        x = X_1col_char$x, 
        y = X_1col_fact$x
    )
    
    X_2col_charchar <- data_frame(
        x = X_1col_char$x, 
        y = X_1col_char2$x
    )
    
    X_2col_lglfact <- data_frame(
        x = X_1col_lgl$x, 
        y = X_1col_fact$x
    )
    
    X_2col_lgllgl <- data_frame(
        x = X_1col_lgl$x, 
        y = X_1col_lgl$x
    )
    
    X_2col_factfact <- data_frame(
        x = X_1col_fact$x, 
        y = X_1col_fact2$x
    )
    
    
    test_that( "Check comparision of equal objects",{
        

        run_mode_test( X_1col_int      , X_1col_int      , 0 )
        run_mode_test( X_1col_int      , X_1col_fact     , 0 )
        run_mode_test( X_1col_int      , X_1col_fact2    , 0 )
        run_mode_test( X_1col_int      , X_1col_num      , 0 )
        run_mode_test( X_1col_int      , X_1col_num2     , 0 )
        run_mode_test( X_1col_int      , X_2col_factfact , 0 )
        run_mode_test( X_1col_num      , X_1col_num      , 0 )
        run_mode_test( X_1col_num      , X_1col_num2     , 0 )
        run_mode_test( X_1col_num      , X_1col_fact     , 0 )
        run_mode_test( X_1col_num      , X_1col_fact2    , 0 )
        run_mode_test( X_1col_num2     , X_1col_fact     , 0 )
        run_mode_test( X_1col_num2     , X_1col_fact2    , 0 )
        run_mode_test( X_1col_char     , X_1col_char     , 0 )
        run_mode_test( X_1col_char     , X_1col_char2    , 0 )
        run_mode_test( X_1col_fact     , X_1col_fact     , 0 )
        run_mode_test( X_1col_fact     , X_1col_fact2    , 0 )
        run_mode_test( X_1col_lgl      , X_1col_lgl      , 0 )
        run_mode_test( X_1col_lgl      , X_1col_lgl2     , 0 )
        run_mode_test( X_2col_numnum   , X_2col_numnum   , 0 )
        run_mode_test( X_2col_numchar  , X_2col_numchar  , 0 )
        run_mode_test( X_2col_numlgl   , X_2col_numlgl   , 0 )
        run_mode_test( X_2col_numfact  , X_2col_numfact  , 0 )
        run_mode_test( X_2col_numnum   , X_2col_numfact  , 0 )
        run_mode_test( X_2col_charlgl  , X_2col_charlgl  , 0 )
        run_mode_test( X_2col_charfact , X_2col_charfact , 0 )
        run_mode_test( X_2col_charchar , X_2col_charchar , 0 )
        run_mode_test( X_2col_lglfact  , X_2col_lglfact  , 0 )
        run_mode_test( X_2col_lgllgl   , X_2col_lgllgl   , 0 )
        run_mode_test( X_2col_factfact , X_2col_factfact , 0 )
        run_mode_test( X_2col_numint   , X_2col_factfact , 0 )
        run_mode_test( X_2col_numnum   , X_2col_factfact , 0 )
        
    })
    

    test_that( "Check comparision of non equal objects aiming for differences of length 1",{
        
        run_mode_test( X_1col_int      , X_1col_char     , 1 )
        run_mode_test( X_1col_int      , X_1col_char2    , 1 )
        run_mode_test( X_1col_int      , X_1col_lgl      , 1 )
        run_mode_test( X_1col_int      , X_1col_lgl2     , 1 )
        run_mode_test( X_1col_int      , X_2col_charlgl  , 1 )
        run_mode_test( X_1col_num      , X_1col_char     , 1 )
        run_mode_test( X_1col_num      , X_1col_char2    , 1 )
        run_mode_test( X_1col_num      , X_1col_lgl      , 1 )
        run_mode_test( X_1col_num      , X_1col_lgl2     , 1 )
        run_mode_test( X_1col_num      , X_2col_charlgl  , 1 )
        run_mode_test( X_1col_num2     , X_1col_char     , 1 )
        run_mode_test( X_1col_num2     , X_1col_char2    , 1 )
        run_mode_test( X_1col_num2     , X_1col_lgl      , 1 )
        run_mode_test( X_1col_num2     , X_1col_lgl2     , 1 )
        run_mode_test( X_1col_num2     , X_2col_charlgl  , 1 )
        run_mode_test( X_1col_char     , X_1col_lgl      , 1 )
        run_mode_test( X_1col_char     , X_1col_lgl2     , 1 )
        run_mode_test( X_1col_char     , X_1col_fact     , 1 )
        run_mode_test( X_1col_char     , X_1col_fact2    , 1 )
        run_mode_test( X_1col_char2    , X_1col_lgl      , 1 )
        run_mode_test( X_1col_char2    , X_1col_lgl2     , 1 )
        run_mode_test( X_1col_char2    , X_1col_fact     , 1 )
        run_mode_test( X_1col_char2    , X_1col_fact2    , 1 )
        run_mode_test( X_1col_lgl      , X_1col_fact     , 1 )
        run_mode_test( X_1col_lgl      , X_1col_fact2    , 1 )
        run_mode_test( X_1col_lgl2     , X_1col_fact     , 1 )
        run_mode_test( X_1col_lgl2     , X_1col_fact2    , 1 )
        run_mode_test( X_2col_numnum   , X_2col_numchar  , 1 )
        run_mode_test( X_2col_numnum   , X_2col_numlgl   , 1 )
        run_mode_test( X_2col_numchar  , X_2col_numlgl   , 1 )
        run_mode_test( X_2col_numchar  , X_2col_numfact  , 1 )
        run_mode_test( X_2col_numlgl   , X_2col_numfact  , 1 )
        run_mode_test( X_2col_charchar , X_2col_charlgl  , 1 )
        run_mode_test( X_2col_charchar , X_2col_charfact , 1 )
        run_mode_test( X_2col_lgllgl   , X_2col_lglfact  , 1 )
        run_mode_test( X_2col_factfact , X_2col_intchar  , 1 )
        run_mode_test( X_2col_numnum   , X_2col_charfact , 1 )

    })
    
    test_that( "Check comparision of non equal objects aiming for differences of length 2",{
        
        run_mode_test( X_2col_numnum   , X_2col_charlgl  , 2 )
        run_mode_test( X_2col_numnum   , X_2col_charchar , 2 )
        run_mode_test( X_2col_numnum   , X_2col_lgllgl   , 2 )
        run_mode_test( X_2col_charchar , X_2col_lglfact  , 2 )
        run_mode_test( X_2col_charchar , X_2col_lgllgl   , 2 )
        run_mode_test( X_2col_charchar , X_2col_factfact , 2 )
        run_mode_test( X_2col_lgllgl   , X_2col_factfact , 2 )
        
    })
    
}


run_mode_test_with_N_obs(1)
run_mode_test_with_N_obs(10)
run_mode_test_with_N_obs(100)
run_mode_test_with_N_obs(1000)

