
test_that("check extra rows",{
    dat1 <- data.table(id = c(1,2,3))
    
    dat2 <- data.table(id = c(5,1,2,3,4))
    
    x <- check_extra_rows_base(dat1, dat1, "id")
    expect_passed(x)
    expect_null(x$data)
    expect_null(x$exclude_rows)
    expect_null(x$exclude_cols)
    
    x <- check_extra_rows_base(dat1, dat2, "id")
    expect_passed(x)
    expect_null(x$data)
    expect_null(x$exclude_rows)
    expect_null(x$exclude_cols)
    
    x <- check_extra_rows_comp(dat1, dat2, "id")
    expect_failed(x)
    expect_equal(x$data$id, c(5,4))
    expect_equal(x$exclude_rows$comp, c(1,5))
    expect_s3_class(x, c("checkResult", "R6"), exact = FALSE)
    expect_s3_class(x$display, c("display", "R6"), exact = FALSE)
    expect_equal( tracemem(x$data), tracemem(x$display$body[[1]]))
    expect_null(x$exclude_rows$base)
    expect_null(x$exclude_cols)
})









