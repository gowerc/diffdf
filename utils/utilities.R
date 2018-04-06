
# library(dplyr)
# library(testthat)


### Most common devtools functions
devtools::load_all()
devtools::test()
devtools::check()
devtools::document()

devtools::build()

### Ensure dfcompare is unloaded and removed
unloadNamespace("dfcompare")
utils::remove.packages("dfcompare")

### Document build and install R compare
devtools::document()
location <- devtools::build()
install.packages( location, repos = NULL, type="source")

### Very simple test
dfcompare::dfcompare(iris,iris)
library(dfcompare)
dfcompare(iris , iris)
?dfcompare

###############
#
#  Ensure R clinical is installed
#

library(rclinical)
?install

devtools::install_git( 
    "https://github.roche.com/Rpackages/dfcompare/tree/fix_dplyr_update", 
    upgrade_dependencies = F
)

rclinical::clindata_names
rclinical::access_data( c("ae" , "vs"))
rclinical::access_data( "ae" )

################
#
# Manually run some tests
#


source("./tests/testthat/helper-create_test_data.R")


devtools::load_all()
dfcompare(iris , iris)
dfcompare(TDAT , TDAT)
dfcompare(TDAT2 , TDAT2)

dfcompare(
    TDAT %>% select(ID , BINARY) ,
    TDAT2 %>% select( ID , BINARY) 
)

dfcompare(
    TDAT %>% select(ID , CATEGORICAL) ,
    TDAT2 %>% select( ID , CATEGORICAL) 
)

dfcompare(
    TDAT %>% select(ID , DATE) ,
    TDAT2 %>% select( ID , DATE) 
)

dfcompare(
    TDAT %>% select(ID , CONTINUOUS) ,
    TDAT2 %>% select( ID , CONTINUOUS) 
)

dfcompare(
    TDAT %>% select(ID , DATETIME) ,
    TDAT2 %>% select( ID , DATETIME) 
)

dfcompare(
    TDAT,
    TDAT2
)

TDAT3 <- TDAT %>% sample_frac(1)
dfcompare(TDAT , TDAT3)

x <- dfcompare(TDAT , TDAT2 , keys = c("ID" , "GROUP1"))
print(x ,VARIABLE =  "GROUP2")

class(x)


dfcompare_has_pass <- function(x){
    if (  class(x)[[1]] != "dfcompare" )  stop( "x is not an dfcompare object")
    return( length(x) == 0 ) 
}



dfcompare_has_pass(x)





dfcompare(TDAT , TDAT2 , outfile = "./testing2.txt")
