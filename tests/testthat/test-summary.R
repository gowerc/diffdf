
test_that("Summar on no differences",{
    d1 <- data.table(
        x = c(1,2,3),
        y = c(1,2,3)
    )
    
    d2 <- data.table(
        x = c(1,2,3),
        y = c(1,2,3)
    )
    
    x <- diffdf(d1, d2, onfailure = "nothing")
    y <- summary(x)
    
    expect_length(y, 0)
    expect_snapshot_output(z <- print(y)) 
    expect_s3_class(z, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
    expect_equal(sum(z$Result == "Failed"), 0)
})


test_that("Summary on simple value differences",{
    d1 <- data.table(
        x = c(1,2,3),
        y = c(1,2,3)
    )
    
    d2 <- data.table(
        x = c(1,2,4),
        y = c(1,2,4)
    )
    
    x <- diffdf(d1, d2, onfailure = "nothing")
    y <- summary(x)
    
    expect_length(y, 1)
    expect_length(y$Values, 2)
    expect_s3_class(y$Values$x, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
    expect_snapshot_output(z <- print(y)) 
    expect_s3_class(z, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
    expect_equal(sum(z$Result == "Failed"), 1)
})


test_that("Summary on multiple differences",{
    
    d1 <- data.table(
        x = c(1,2,3),
        y = c(1,2,3)
    )
    
    d2 <- data.table(
        x = c(1,2,4),
        y = list(c(1,2), c(3,4), c(5,6)),
        z = c("1","2","3")
    )
    
    x <- diffdf(d1, d2, onfailure = "nothing")
    y <- summary(x)
    
    expect_length(y, 3)
    expect_length(y$Values, 1)
    expect_s3_class(y$Values$x, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
    expect_s3_class(y$ExtraColsComp, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
    expect_s3_class(y$Mode, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
    expect_snapshot_output(z <- print(y)) 
    expect_s3_class(z, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
    expect_equal(sum(z$Result == "Failed"), 3)
})





















