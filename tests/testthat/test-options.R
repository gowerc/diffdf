# TODO
test_that("Unexpected options error",{
    msg <- "The following options are invalid: notanoption alsonotanoption"
    expect_error( diffdf(TDAT, TDAT, notanoption = TRUE, alsonotanoption = TRUE), msg)
    expect_error( diffdf(TDAT, TDAT, opts = diffopts(notanoption = TRUE, alsonotanoption = TRUE)), msg)
})


test_that("Depreciated options error",{
    msg <- "`suppress_warnings = TRUE` is depreciated as of diffdf v2.0.0\nPlease use `onfailure = 'nothing'` instead"
    expect_error(diffdf(TDAT, TDAT, suppress_warnings = TRUE), msg)
})
