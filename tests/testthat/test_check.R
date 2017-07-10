
# library(dplyr)
# library(readr)


TDAT <- read_csv (
    paste0( system.file(package="rcompare" , "testdata"), "/test1.csv"),
    col_types =  cols(
        ID          = col_integer(),
        GROUP1      = col_integer(),
        GROUP2      = col_integer(),
        INTEGER     = col_integer(),
        BINARY      = col_factor(c("M","F")),
        DATE        = col_date("%d/%m/%Y"),
        CONTINUOUS  = col_double(),
        CATEGORICAL = col_factor(c("A","B","C","D")),
        LOGICAL     = col_logical(),
        CHARACTER   = col_character()
    )
)


test_that( "Check comparision of equal objects",{
    expect_equal(1,1)
})


