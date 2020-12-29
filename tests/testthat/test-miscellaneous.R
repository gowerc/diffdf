

test_that( "sort order is as expected (sorted by keys)", {
    
    dsin1 <- data.frame(
        G1 = rep(c(1,10,2,101) , 3),
        G2 = as.Date(c(
            "2015-10-12" , "2015-10-12" , "2015-10-12" ,"2015-10-12",
            "2015-11-12" , "2015-11-12" , "2015-11-12" ,"2015-11-12",
            "2015-12-12" , "2015-12-12" , "2015-12-12" ,"2015-12-12"
        )),
        var = c(9,2,5,2,3,10,12,42,1,2,8, 54)
    )
    dsin2 <- dsin1
    dsin2$var[c(1,2,3)] <- c(97, 98,99)
    
    x <- diffdf(dsin1,dsin2, keys = c("G1"  , "G2") , onfailure = "nothing")
    y <- summary(x)
    
    expect_true( all( y$Values$var$G1 == c(1,2,10)))
    expect_true( all( y$Values$var$Compare == c( 97,99,98)))
})



test_that("Null datasets work as expected", {
    
    x <- diffdf( data.frame(), data.frame())
    expect_equal(x$result, "Passed")
    
    
    df <- data.frame(x = c(1,2,3,4,5), y = c("a", "b", "c", "d", "e"))
    x <- diffdf( df, data.frame(), onfailure = "nothing")
    expect_equal(x$result, "Failed")
    y <- summary(x)
    expect_equal(nrow(y$ExtraRowsBase), 5)
    expect_equal(nrow(y$ExtraColsBase), 2)
    
    dat1 <- data.frame()
    dat2 <- data.frame()
    
    
    expect_passed( diffdf( dat1, dat2))
    expect_failed( diffdf(dat1, TDAT, onfailure = "nothing"))
 
    y <- summary(diffdf(dat1, TDAT, onfailure = "nothing"))
    expect_length(y$ExtraColsComp$Columns, ncol(TDAT))
    expect_length(y$ExtraRowsComp$..ROWNUMBER.., nrow(TDAT))
    
    y <- summary(diffdf ( TDAT[0,c(1,2,3)], TDAT, keys =  c("GROUP1", "GROUP2"), onfailure = "nothing"))
    expect_length(y$ExtraRowsComp$GROUP1,  nrow(TDAT) )
    expect_length(y$ExtraColsComp$Columns,  ncol(TDAT) - 3 )
    
})




