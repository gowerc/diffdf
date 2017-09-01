library(purrr)
library(dplyr)
library(readr)
library(testthat)
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

TDAT_twovars_twoatts <- TDAT

attr(TDAT_twovars_twoatts$INTEGER, 'attone') <- 'character'
attr(TDAT_twovars_twoatts$INTEGER, 'atttwo') <- list(data.frame(x = rnorm(21),
                                                                y = rnorm(21)),
                                                     c(1,2,3))
attr(TDAT_twovars_twoatts$BINARY, 'attone') <- rnorm(100)
attr(TDAT_twovars_twoatts$BINARY, 'atttwo') <- tibble(list(data.frame(x = rnorm(11),
                                                                y = rnorm(11))),
                                                     c(1))


TDAT_twovars_twoatts2 <- TDAT

attr(TDAT_twovars_twoatts2$INTEGER, 'attone') <- letters[1:4]
attr(TDAT_twovars_twoatts2$INTEGER, 'atttwo') <- data.frame(x=rnorm(10),
                                                            y=rnorm(10))
attr(TDAT_twovars_twoatts2$BINARY, 'attone') <- c(T,F,T)
attr(TDAT_twovars_twoatts2$BINARY, 'atttwo') <- c(1,2,3)


TDAT_twovars_twoatts2a <- TDAT_twovars_twoatts2

attr(TDAT_twovars_twoatts2a$INTEGER, 'attone') <- 'test'
attr(TDAT_twovars_twoatts2a$BINARY, 'attone') <- runif(3)


TDAT_twovars_twoatts_diffname <- TDAT

attr(TDAT_twovars_twoatts_diffname$INTEGER, 'attone2') <- letters[1:4]
attr(TDAT_twovars_twoatts_diffname$INTEGER, 'atttwo2') <- data.frame(x=rnorm(10),
                                                            y=rnorm(10))
attr(TDAT_twovars_twoatts_diffname$BINARY, 'attone2') <- c(T,F,T)
attr(TDAT_twovars_twoatts_diffname$BINARY, 'atttwo2') <- c(1,2,3)

rowcount <- function(d1, d2, value_compare){
  nrow(att_diffs(d1, d2 , names(TDAT))) == value_compare
}


test_that( "Check comparision of equal objects",{
  
  expect_true(rowcount(TDAT_twovars_twoatts, TDAT_twovars_twoatts, 0),
              'Check twoatts compares equal to itself')
  expect_true(rowcount(TDAT_twovars_twoatts2, TDAT_twovars_twoatts2, 0),
              'Check twoatts2 compares equal to itself')
  expect_true(rowcount(TDAT_twovars_twoatts2a, TDAT_twovars_twoatts2a, 0),
              'Check twoatts2a compares equal to itself')
  expect_true(rowcount(TDAT_twovars_twoatts_diffname, TDAT_twovars_twoatts_diffname, 0),
              'Check twoatts_diffname compares equal to itself')
})

test_that( "Check comparision of unequal objects",{
  
  expect_true(rowcount(TDAT_twovars_twoatts, TDAT_twovars_twoatts2, 4), 'compare twoatts with twoatts2')
  expect_true(rowcount(TDAT_twovars_twoatts, TDAT_twovars_twoatts2a, 4), 'compare twoatts with twoatts2a')
  expect_true(rowcount(TDAT_twovars_twoatts2, TDAT_twovars_twoatts2a, 2), 'compare twoatts2 with twoatts2a')
  expect_true(rowcount(TDAT_twovars_twoatts, TDAT_twovars_twoatts_diffname, 8), 'compare twoatts with twoatts_diffname')
  expect_true(rowcount(TDAT_twovars_twoatts2, TDAT_twovars_twoatts_diffname, 8), 'compare twoatts2 with twoatts_diffname')
  expect_true(rowcount(TDAT_twovars_twoatts2a, TDAT_twovars_twoatts_diffname, 8), 'compare twoatts2a with twoatts_diffname')
})


