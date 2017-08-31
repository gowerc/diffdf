library(purrr)
library(dplyr)
library(readr)
library(testthat)

  N<-10

  X_1col_num <- data.frame(x = seq(1, N))
 
  X_1col_char <- data.frame(x = letters[1:N], stringsAsFactors = FALSE)
  X_1col_char2 <- data.frame(x = rep('cat', N), stringsAsFactors = FALSE)
  
  X_1col_fact <- data.frame(x = letters[1:N])
  X_1col_fact2 <- data.frame(x = factor(rep(c('cat', 'dog'), c(floor(N/2), ceiling(N/2)))))
  
  
  X_1col_logical <- data.frame(x = rep(T, N))
  X_1col_logical2 <- data.frame(x = rep(c(T,F), c(floor(N/2), ceiling(N/2))))
  
  X_2col_numchar <- tibble(x = X_1col_num$x, y = X_1col_char$x)
  X_2col_numnum <- tibble(x = X_1col_num$x, y = X_1col_num2$x)
  X_2col_numlog <- tibble(x = X_1col_num$x, y = X_1col_logical$x)
  X_2col_numfact <- tibble(x = X_1col_num$x, y = X_1col_fact$x)
  X_2col_numfact2 <- tibble(x = X_1col_num$x, y = X_1col_fact2$x)
  
  X_2col_charlog <- tibble(x = X_1col_char$x, y = X_1col_logical$x)
  X_2col_charfact <- tibble(x = X_1col_char$x, y = X_1col_fact$x)
  X_2col_charfact2 <- tibble(x = X_1col_char$x, y = X_1col_fact2$x)
  X_2col_charchar <- tibble(x = X_1col_char$x, y = X_1col_char2$x)
  
  X_2col_logfact <- tibble(x = X_1col_logical$x, y = X_1col_fact$x)
  X_2col_logfact2 <- tibble(x = X_1col_logical$x, y = X_1col_fact2$x)
  X_2col_loglog <- tibble(x = X_1col_logical$x, y = X_1col_logical2$x)
  
  X_2col_factfact <- tibble(x = X_1col_fact$x, y = X_1col_fact2$x)
  X_2col_factfact2 <- tibble(y =X_1col_fact$x, x = X_1col_fact2$x)
  
  test_that( "Check comparision of equal objects",{
    
    expect_true(nrow(factlevels(X_1col_num , X_1col_num , 'x')) == 0)
    expect_true(nrow(factlevels(X_1col_num , X_1col_num2 , 'x')) == 0)
    
    expect_true(nrow(factlevels(X_1col_char , X_1col_char , 'x')) == 0)
    expect_true(nrow(factlevels(X_1col_char , X_1col_char2 , 'x')) == 0)
    
    expect_true(nrow(factlevels(X_1col_fact , X_1col_fact, 'x')) == 0)
    expect_true(nrow(factlevels(X_1col_fact2 , X_1col_fact2 , 'x')) == 0)
    
    expect_true(nrow(factlevels(X_1col_logical , X_1col_logical , 'x')) == 0)
    expect_true(nrow(factlevels(X_1col_logical , X_1col_logical2 , 'x')) == 0)
    
    expect_true(nrow(factlevels(X_2col_numnum , X_2col_numnum , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_numchar , X_2col_numchar , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_numlog , X_2col_numlog , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_numfact , X_2col_numfact , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_charlog , X_2col_charlog , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_charfact , X_2col_charfact , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_charchar , X_2col_charchar , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_logfact , X_2col_logfact , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_loglog , X_2col_loglog , c('x','y'))) == 0)
    expect_true(nrow(factlevels(X_2col_factfact , X_2col_factfact , c('x','y'))) == 0)
  })
  
  test_that( "Check comparision of non equal objects aiming for differences of length 1",{
    
    
    expect_true(nrow(factlevels(X_1col_fact,  X_1col_fact2  , 'x')) == 1)
    expect_true(nrow(factlevels(X_2col_numfact , X_2col_numfact2 , c('x','y'))) == 1)
    expect_true(nrow(factlevels(X_2col_charfact , X_2col_charfact2 , c('x','y'))) == 1)
    expect_true(nrow(factlevels(X_2col_logfact , X_2col_logfact2, c('x','y'))) == 1)
    
    
  })
  
  test_that( "Check comparision of non equal objects aiming for differences of length 2",{
    
    expect_true(nrow(factlevels(X_2col_factfact , X_2col_factfact2 , c('x','y'))) == 2)
    
  })
  


