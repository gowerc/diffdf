context("Testing how function handles nulldatasets")

dat1 <- data.frame()
dat2 <- data.frame()



test_that( "Simple results are as expected", {
    
    expect_length( 
        diffdf( dat1, dat2), 
        0 
    )
    
    expect_length( 
        diffdf(dat1, iris, warnings = FALSE), 
        2
    )
    
    expect_length( 
        diffdf(dat1, iris, warnings = FALSE)$ExtColsComp$COLUMNS  , 
        5
    )
    
})



test_that( "Missing data of a dataset that should exist", {
    
    expect_length( 
        diffdf ( TDAT[0,c(1,2,3)], TDAT, keys =  c("GROUP1", "GROUP2"), warnings = FALSE)$ExtRowsComp$GROUP1, 
        20
    )
    
    expect_length( 
        diffdf ( TDAT[0,c(1,2,3)], TDAT, keys =  c("GROUP1", "GROUP2"), warnings = FALSE)$ExtColsComp$COLUMNS
        , 8
    )
    
})




