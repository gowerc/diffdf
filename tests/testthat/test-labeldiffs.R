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

labeldiffs <-function(BASE, COMP, matching_cols){
  attdiffs(BASE, COMP, matching_cols, 'label')
}

TDATonelabel <- TDAT
attr(TDATonelabel $INTEGER, 'label') <- 'test'

TDATonelabel2 <- TDAT
attr(TDATonelabel2$INTEGER, 'label') <- 'different'

TDATtwolabel <- TDAT 
attr(TDATtwolabel$INTEGER, 'label') <- 'test'
attr(TDATtwolabel$BINARY, 'label') <- 'test2'

TDATtwolabel2 <- TDAT 
attr(TDATtwolabel2$INTEGER, 'label') <- 'different'
attr(TDATtwolabel2$BINARY, 'label') <- 'different2'

TDATtwolabel3 <- TDAT 
attr(TDATtwolabel3$INTEGER, 'label') <- 'test'
attr(TDATtwolabel3$BINARY, 'label') <- 'different2'

  
  test_that( "Check comparision of equal objects",{
    
    expect_true(nrow(labeldiffs(TDATonelabel, TDATonelabel, names(TDAT))) == 0)
    expect_true(nrow(labeldiffs(TDATonelabel2, TDATonelabel2, names(TDAT))) == 0)
    expect_true(nrow(labeldiffs(TDATtwolabel2, TDATtwolabel2, names(TDAT))) == 0)
    expect_true(nrow(labeldiffs(TDATtwolabel, TDATtwolabel, names(TDAT))) == 0)
    expect_true(nrow(labeldiffs(TDATtwolabel3, TDATtwolabel3, names(TDAT))) == 0)

  })
  
  test_that( "Check comparision of non equal objects aiming for differences of length 1",{
    
    
    expect_true(nrow(labeldiffs(TDATonelabel, TDATonelabel2, names(TDAT))) == 1)
    expect_true(nrow(labeldiffs(TDATtwolabel2, TDATtwolabel3, names(TDAT))) == 1)
    expect_true(nrow(labeldiffs(TDATtwolabel3, TDATtwolabel, names(TDAT))) == 1)
    
    
  })
  
  test_that( "Check comparision of non equal objects aiming for differences of length 2",{
    
    expect_true(nrow(labeldiffs(TDATtwolabel2, TDATtwolabel, names(TDAT))) == 2)
    
  })
  


