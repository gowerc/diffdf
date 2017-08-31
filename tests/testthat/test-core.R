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

set.seed(101010223)

## Arbitary change some values
TDAT2 <- TDAT %>%
    mutate( INTEGER = ifelse(runif(nrow(.)) >0.8 , INTEGER + 1 , INTEGER))

## Change other values to test
TDAT_CHARCHANGE <- TDAT %>% mutate(CHARACTER = ifelse(row_number() == 1, 'different', CHARACTER))
TDAT_DATECHANGE <- TDAT %>% 
  mutate(DATE = as.Date(ifelse(row_number() == 1, as.Date("01/01/1981", format = "%d/%m/%Y"), DATE),
                        origin = "1970-01-01"))
TDAT_LOGCHANGE <- TDAT %>% 
  mutate(LOGICAL = ifelse(LOGICAL, F, T))

TDAT_FACTVALCHANGE <- TDAT %>% 
  mutate(CATEGORICAL = factor(ifelse(CATEGORICAL == 'A', 'B', as.character(CATEGORICAL)),
                              levels = c('A', 'B','C','D')))

## add nas



TDAT_MODECHANGE <- TDAT %>%
  mutate( INTEGER = as.character(INTEGER))


TDAT_FACTCHANGE <- TDAT %>%
  mutate( CATEGORICAL = factor(CATEGORICAL, levels = c(levels(CATEGORICAL), 'New')))

#switch integer to double, this shouldn't affect the comparison
TDAT_MODEDBL <- TDAT %>%
  mutate( INTEGER = as.double(INTEGER))

test_that( "Check comparision of equal objects",{
    expect_false( rcompare(iris, iris)$Issues )
    expect_false( rcompare(TDAT, TDAT)$Issues )
    expect_false( rcompare(TDAT, TDAT, "ID")$Issues )
    expect_false( rcompare(TDAT, TDAT, c("GROUP1" , "GROUP2"))$Issues )
    expect_false( rcompare(TDAT, TDAT_MODEDBL)$Issues )
})

test_that( "Unequal objects error" , {
    expect_warning( rcompare(TDAT , TDAT2), 'Not all values compared equal' )
  expect_warning( rcompare(TDAT , TDAT_CHARCHANGE), 'Not all values compared equal' )
  expect_warning( rcompare(TDAT , TDAT_DATECHANGE), 'Not all values compared equal' )
  expect_warning( rcompare(TDAT , TDAT_LOGCHANGE), 'Not all values compared equal' )
  expect_warning( rcompare(TDAT , TDAT_FACTVALCHANGE), 'Not all values compared equal' )
})

test_that( "Differing modes error" , {
  expect_warning( rcompare(TDAT , TDAT_MODECHANGE ), 'There are Columns in BASE and COMPARE with different modes' )
})

test_that( "Differing factor levels error" , {
  expect_warning( rcompare(TDAT , TDAT_FACTCHANGE ), 'There are Factor Columns in BASE and COMPARE with different levels' )
})


test_that("Non-Unique rows error", {
    expect_error( rcompare(TDAT , TDAT , "GROUP1"), 'BY variables in BASE do not result in unique observations' )
})
