
library(dplyr)
library(testthat)

devtools::load_all()
devtools::test()
devtools::check()
devtools::document()


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


rcompare(iris , iris)
rcompare(TDAT , TDAT)
rcompare(TDAT2 , TDAT2)



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

rcompare(
    TDAT,
    TDAT2
)

x <- rcompare(TDAT , TDAT2 , keys = c("ID" , "GROUP1"))
print(x ,VARIABLE =  "GROUP2")

