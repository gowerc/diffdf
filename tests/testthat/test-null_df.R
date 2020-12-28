

test_that("Null datasets work as expected", {
    
    x <- diffdf( data.frame(), data.frame())
    expect_equal(x$result, "Passed")
    
    
    df <- data.frame(x = c(1,2,3,4,5), y = c("a", "b", "c", "d", "e"))
    x <- diffdf( df, data.frame(), onfailure = "nothing")
    expect_equal(x$result, "Failed")
    y <- summary(x)
    expect_equal(nrow(y$ExtraRowsBase), 5)
    expect_equal(nrow(y$ExtraColsBase), 2)
})




