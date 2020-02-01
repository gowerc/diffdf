context("Testing how function handles nulldatasets")

dat1 <- data.frame()
dat2 <- data.frame()

test_that( "Simple results are as expected", {
    expect_length( diffdf( dat1, dat2), 0 )
    expect_length( diffdf(dat1, iris, warnings = FALSE), 2)
    expect_length( diffdf(dat1, iris, warnings = FALSE)$ExtColsComp$COLUMNS  , 5)
})
