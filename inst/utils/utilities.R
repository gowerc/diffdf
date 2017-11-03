


devtools::load_all()
devtools::test()
devtools::check()



# devtools::install( "./" , dependencies = F)
# devtools::install( "./" , dependencies = F , local = F)

unloadNamespace("rcompare")
utils::remove.packages("rcompare")

devtools::document()
location <- devtools::build()

install.packages( location, repos = NULL, type="source")

rcompare::rcompare(iris,iris)
library(rcompare)
rcompare(iris , iris)



source("./tests/testthat/helper-create_test_data.R")
### Setup test data
TDAT2 <- TDAT

## Unequal values
TDAT2$CONTINUOUS[c(1,5,7)] <- c( 1,2,3)

## Different attributes
attr(TDAT2$BINARY , "something") <- iris

## Different levels
levels(TDAT2$CATEGORICAL) <- c("A", "B" , "D")

## Different class
class( TDAT2$DATE) <-  c("A_DATE" , "b_date" , "cDate")

## Different mode
TDAT2$INTEGER[c(1,5,7)] <- c("1" , "2" , "3")

attr(TDAT2$DATETIME , "label") <- "This is the label for my amazing variable"



rcompare(
    TDAT %>% select(ID , BINARY) ,
    TDAT2 %>% select( ID , BINARY) 
)

rcompare(
    TDAT %>% select(ID , CATEGORICAL) ,
    TDAT2 %>% select( ID , CATEGORICAL) 
)

rcompare(
    TDAT %>% select(ID , DATE) ,
    TDAT2 %>% select( ID , DATE) 
)

rcompare(
    TDAT %>% select(ID , CONTINUOUS) ,
    TDAT2 %>% select( ID , CONTINUOUS) 
)

rcompare(
    TDAT %>% select(ID , DATETIME) ,
    TDAT2 %>% select( ID , DATETIME) 
)
rcompare(TDAT , TDAT2)
