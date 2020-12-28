

context("Testing attribute differnce functions")

##### Setup test data with attributes ----
#
#
#
#


TDAT_twovars_twoatts <- TDAT
attr(TDAT_twovars_twoatts$INTEGER, 'attone') <- 'character'
attr(TDAT_twovars_twoatts$INTEGER, 'atttwo') <- list(data.frame(x = rnorm(21),y = rnorm(21)),c(1,2,3))
attr(TDAT_twovars_twoatts$BINARY,  'attone') <- rnorm(100)
attr(TDAT_twovars_twoatts$BINARY,  'atttwo') <- tibble(list(data.frame(x = rnorm(11),y = rnorm(11))),c(1))


TDAT_twovars_twoatts2 <- TDAT
attr(TDAT_twovars_twoatts2$INTEGER, 'attone') <- letters[1:4]
attr(TDAT_twovars_twoatts2$INTEGER, 'atttwo') <- data.frame(x=rnorm(10),y=rnorm(10))
attr(TDAT_twovars_twoatts2$BINARY,  'attone') <- c(T,F,T)
attr(TDAT_twovars_twoatts2$BINARY,  'atttwo') <- c(1,2,3)


TDAT_twovars_twoatts2a <- TDAT_twovars_twoatts2
attr(TDAT_twovars_twoatts2a$INTEGER, 'attone') <- 'test'
attr(TDAT_twovars_twoatts2a$BINARY,  'attone') <- runif(3)


TDAT_twovars_twoatts_diffname <- TDAT
attr(TDAT_twovars_twoatts_diffname$INTEGER, 'attone2') <- letters[1:4]
attr(TDAT_twovars_twoatts_diffname$INTEGER, 'atttwo2') <- data.frame( x=rnorm(10),y=rnorm(10))
attr(TDAT_twovars_twoatts_diffname$BINARY,  'attone2') <- c(T,F,T)
attr(TDAT_twovars_twoatts_diffname$BINARY,  'atttwo2') <- c(1,2,3)


##### Perform tests ----
#
#
#
#

run_att_test <- function( dsin1, dsin2 , expect_diff ){
    CALL  <- match.call()
    expect_equal( 
        identify_att_differences(dsin1, dsin2) %>% nrow,
        expect_diff,
        info = stringr::str_c("dsin1 = " , as.character(CALL[2]) , "\ndsin2 = " , as.character(CALL[3])) ,
        label =  "identify_mode_differences returns a row count"
    )
}

test_that( "Check comparision of attributes objects",{
    
    run_att_test(TDAT_twovars_twoatts,          TDAT_twovars_twoatts,          0)
    run_att_test(TDAT_twovars_twoatts2,         TDAT_twovars_twoatts2,         0)
    run_att_test(TDAT_twovars_twoatts2a,        TDAT_twovars_twoatts2a,        0)
    run_att_test(TDAT_twovars_twoatts_diffname, TDAT_twovars_twoatts_diffname, 0)
    run_att_test(TDAT_twovars_twoatts,          TDAT_twovars_twoatts2 ,        4)
    run_att_test(TDAT_twovars_twoatts,          TDAT_twovars_twoatts2a,        4)
    run_att_test(TDAT_twovars_twoatts2,         TDAT_twovars_twoatts2a,        2)
    run_att_test(TDAT_twovars_twoatts,          TDAT_twovars_twoatts_diffname, 8)
    run_att_test(TDAT_twovars_twoatts2,         TDAT_twovars_twoatts_diffname, 8)
    run_att_test(TDAT_twovars_twoatts2a,        TDAT_twovars_twoatts_diffname, 8)

})



# x <- identify_att_differences( TDAT_twovars_twoatts , TDAT_twovars_twoatts2a )

