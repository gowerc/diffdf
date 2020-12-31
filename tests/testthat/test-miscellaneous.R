

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


test_that("Data.Table doesn't modify input dataset",{
    
    dat1 <- data.table(
        id = c(1,2),
        v1 = c("a", "b"),
        v2 = c(1L , 2L),
        v3 = list("ab", 1),
        v4 = c(123, 123)
    )
    
    dat2 <- data.table(
        id = c(1,3),
        v1 = c("c", "b"),
        v2 = c(3L , 2L),
        v3 = list("fe", 1),
        v4 = c(124, 123)
    )
    
    init_dat1 <- copy(dat1)
    init_dat2 <- copy(dat2)  
    
    x <- diffdf(dat1, dat2, "id", onfailure = "nothing")
    expect_failed(x)
    expect_false( tracemem(dat1) == tracemem(init_dat1))
    expect_false( tracemem(dat2) == tracemem(init_dat2))
    expect_true( identical(dat1, init_dat1))
    expect_true( identical(dat1, init_dat1))
    
    
})



