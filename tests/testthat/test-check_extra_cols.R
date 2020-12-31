
test_that("Check Extra Columns", {
    dat1 <- data.table(
        x = c(1,2,3),
        y = c("a", "b", "c")
    )
    
    dat2 <- data.table(
        x = c(1,2,3),
        y = c(1,2,3)
    )
    
    dat3 <- data.table(
        x = c(1,2,3),
        y = c("a", "b", "c"),
        z = c(1,2,3),
        w = c(1,2,3)
    )
    
    dat4 <- data.table(
        x = c(1,2,3),
        y = c("a", "b", "c"),
        w = c(1,2,3),
        z = c(1,2,3)
    )
    
    x <- check_extra_cols_base(dat1, dat1)
    expect_passed(x)
    expect_null(x$data)
    expect_s3_class(x, c("checkResult", "R6") , exact = TRUE)
    expect_s3_class(x$display, c("display", "R6") , exact = TRUE)
    
    
    x <- check_extra_cols_base(dat1, dat2)
    expect_passed(x)
    expect_null(x$data)
    
    
    x <- check_extra_cols_base(dat1, dat3)
    expect_passed(x)
    expect_null(x$data)
    
    
    x <- check_extra_cols_comp(dat1, dat3)
    expect_failed(x)
    expect_equal(x$data$Columns, c("z", "w"))
    
    
    x <- check_extra_cols_comp(dat1, dat4)
    expect_failed(x)
    expect_equal(x$data$Columns, c("w", "z"))
    expect_s3_class(x, c("checkResult", "R6") , exact = TRUE)
    expect_s3_class(x$display, c("display", "R6") , exact = TRUE)
    expect_equal(tracemem(x$data), tracemem(x$display$body[[1]]))
    
})

