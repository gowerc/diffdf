
run_att_test <- function( dsin1, dsin2 , diff ){
    CALL  <- match.call()
    
    info <- sprintf(
        "dsin1 = %s\ndsin2 = %s", 
        as.character(CALL[2]),
        as.character(CALL[3])
    )
    
    x <- check_attributes(dsin1, dsin2, "ID")
    
    
    if( diff == 0){
        expected_result <- "Passed"
        expected_value <- NULL
    } else {
        expected_result <- "Failed"
        expected_value <- diff
    }

    expect_s3_class( x , c("checkResult", "R6"), exact = TRUE)
    expect_equal( nrow(x$data), expected_value, info =  info)
    expect_equal( x$result, expected_result, info =  info)
    
    expect_null(x$exclude_cols)
    expect_null(x$exclude_rows)

    expect_s3_class( x$display , c("display", "R6"), exact = TRUE)
    if( expected_result == "Failed"){
        expect_equal( tracemem(x$display$body[[1]]) , tracemem(x$data), info =  info)
    }
    
}


test_that( "Check comparision of attributes objects",{
    
    
    TDAT_twovars_twoatts <- TDAT
    attr(TDAT_twovars_twoatts$INTEGER, 'attone') <- 'character'
    attr(TDAT_twovars_twoatts$INTEGER, 'atttwo') <- list(data.frame(x = rnorm(21),y = rnorm(21)),c(1,2,3))
    attr(TDAT_twovars_twoatts$BINARY,  'attone') <- rnorm(100)
    attr(TDAT_twovars_twoatts$BINARY,  'atttwo') <- data.table(list(data.frame(x = rnorm(11),y = rnorm(11))),c(1))
    
    
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
