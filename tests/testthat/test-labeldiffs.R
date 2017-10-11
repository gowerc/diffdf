# library(purrr)
# library(dplyr)
# library(testthat)
# source("./tests/testthat/helper-create_test_data.R")


labeldiffs <-function(BASE, COMP, matching_cols){
    attdiffs(BASE, COMP, matching_cols, 'label')
}


TDAT_onelabel <- TDAT
attr(TDAT_onelabel $INTEGER, 'label') <- 'test'


TDAT_onelabel2 <- TDAT
attr(TDAT_onelabel2$INTEGER, 'label') <- 'different'


TDAT_twolabel <- TDAT 
attr(TDAT_twolabel$INTEGER, 'label') <- 'test'
attr(TDAT_twolabel$BINARY, 'label') <- 'test2'


TDAT_twolabel2 <- TDAT 
attr(TDAT_twolabel2$INTEGER, 'label') <- 'different'
attr(TDAT_twolabel2$BINARY, 'label') <- 'different2'


TDAT_twolabel3 <- TDAT 
attr(TDAT_twolabel3$INTEGER, 'label') <- 'test'
attr(TDAT_twolabel3$BINARY, 'label') <- 'different2'





test_that( "Check comparision of equal objects", {
    
    expect_equal(
        labeldiffs(TDAT_onelabel,  TDAT_onelabel,  names(TDAT)) %>% nrow, 
        0
    )
    
    expect_equal(
        labeldiffs(TDAT_onelabel2, TDAT_onelabel2, names(TDAT)) %>% nrow, 
        0
    )
    
    expect_equal(
        labeldiffs(TDAT_twolabel2, TDAT_twolabel2, names(TDAT)) %>% nrow, 
        0
    )
    
    expect_equal(
        labeldiffs(TDAT_twolabel,  TDAT_twolabel,  names(TDAT)) %>% nrow, 
        0
    )
    
    expect_equal(
        labeldiffs(TDAT_twolabel3, TDAT_twolabel3, names(TDAT)) %>% nrow,
        0
    )
    
})




test_that( "Check comparision of non equal objects aiming for differences of length 1",{
    
    expect_equal(
        labeldiffs(TDAT_onelabel,  TDAT_onelabel2, names(TDAT)) %>% nrow, 
        1
    )
    
    expect_equal(
        labeldiffs(TDAT_twolabel2, TDAT_twolabel3, names(TDAT)) %>% nrow, 
        1
    )
    
    expect_equal(
        labeldiffs(TDAT_twolabel3, TDAT_twolabel,  names(TDAT)) %>% nrow,  
        1
    )
    
})




test_that( "Check comparision of non equal objects aiming for differences of length 2",{
    
    expect_equal(
        labeldiffs(TDAT_twolabel2, TDAT_twolabel, names(TDAT)) %>% nrow,
        2
    )
    
})



