
TDAT_CAT_ASCHR <- TDAT
TDAT_CAT_ASCHR$CATEGORICAL <- as.character(TDAT_CAT_ASCHR$CATEGORICAL)


TDAT_INT_ASNUM <- TDAT
TDAT_INT_ASNUM$INTEGER <- as.numeric(TDAT_INT_ASNUM$INTEGER)



test_that("Factor cast to character, no other changes!", {
    x <- cast_variables(TDAT, TDAT_INT_ASNUM, cast_factors = TRUE)
    expect_equal(x$BASE, TDAT)
    expect_equal(x$COMPARE, TDAT_INT_ASNUM)

    suppressMessages({
        x <- cast_variables(TDAT, TDAT_CAT_ASCHR, cast_factors = TRUE)
    })
    expect_equal(x$BASE, TDAT_CAT_ASCHR)
    expect_equal(x$COMPARE, TDAT_CAT_ASCHR)
})



test_that("Int cast to numeric, no other changes!", {
    x <- cast_variables(TDAT, TDAT_CAT_ASCHR, cast_integers = TRUE)
    expect_equal(x$BASE, TDAT)
    expect_equal(x$COMPARE, TDAT_CAT_ASCHR)

    suppressMessages({
        x <- cast_variables(TDAT, TDAT_INT_ASNUM, cast_integers = TRUE)
    })
    expect_equal(x$BASE, TDAT_INT_ASNUM)
    expect_equal(x$COMPARE, TDAT_INT_ASNUM)
})
