


test_that("Basic usage",{
    x <- check_class(TDAT, TDAT)
    expect_passed(x)
    expect_null(x$exclude_cols)
    expect_null(x$exclude_rows)
    expect_s3_class(x, c("checkResult", "R6"), exact = TRUE)
})



test_that("Specific Values",{
    dat1 <- TDAT
    dat2 <- TDAT
    
    class(dat1$ID) <- "string"
    class(dat2$ID) <- c("not", "string")
    
    class(dat1$GROUP1) <- "abc"
    class(dat2$GROUP2) <- "def"
    
    x <- check_class(dat1, dat2)
    
    expect_failed(x)
    
    expect_equal( x$data$Base, list("abc", "integer", "string"))
    expect_equal( x$data$Compare, list("numeric", "def", c("not", "string")))
    
    expect_equal( x$exclude_cols, c("GROUP1", "GROUP2", "ID"))
    expect_null(x$exclude_rows)
    expect_s3_class(x, c("checkResult", "R6"), exact = TRUE)
    
    
    expect_s3_class(x$display, c("display", "R6"), exact = TRUE)
    expect_equal(x$display$body[[1]], x$data)
    
    expect_equal( tracemem(x$display$body[[1]]) , tracemem(x$data))
    
    
})    
    


test_that("List Columns",{
    x <- check_class( data.table(x = list(c(1,2,3))), data.table(x = list(c(1,2,3))))
    expect_passed(x)
    
    x <- check_class( data.table(x = list(c(1,2,3))), data.table(x = c(1,2,3)))
    expect_failed(x)
    expect_equal( x$data$Base, list("list"))
    expect_equal( x$data$Compare, list("numeric"))
})

