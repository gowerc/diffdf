

context( "Testing misc (currently sorting only)")


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
    
    x <- diffdf(dsin1,dsin2, keys = c("G1"  , "G2") , suppress_warnings = T)
    x2 <- diffdf_issuerows(dsin2, x)
    
    expect_true( all( x$VarDiff_var$G1 == c(1,2,10)))
    expect_true( all( x$VarDiff_var$COMPARE == c( 97,99,98)))
    expect_true( all( x2$G1 == c(1,2,10) ))
    expect_true( all( x2$var == c( 97,99,98)))
})


