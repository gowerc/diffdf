
# library(dplyr)
# library(testthat)


### Most common devtools functions
devtools::load_all()
devtools::test()
devtools::check()
devtools::document()

devtools::build()

### Ensure rcompare is unloaded and removed
unloadNamespace("rcompare")
utils::remove.packages("rcompare")

### Document build and install R compare
devtools::document()
location <- devtools::build()
install.packages( location, repos = NULL, type="source")

### Very simple test
rcompare::rcompare(iris,iris)
library(rcompare)
rcompare(iris , iris)
?rcompare

###############
#
#  Ensure R clinical is installed
#

library(rclinical)
?install

devtools::install_git( 
    "https://github.roche.com/Rpackages/rcompare/tree/fix_dplyr_update", 
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

TDAT3 <- TDAT %>% sample_frac(1)
rcompare(TDAT , TDAT3)

x <- rcompare(TDAT , TDAT2 , keys = c("ID" , "GROUP1"))
print(x ,VARIABLE =  "GROUP2")

class(x)


rcompare_has_pass <- function(x){
    if (  class(x)[[1]] != "rcompare" )  stop( "x is not an rcompare object")
    return( length(x) == 0 ) 
}



rcompare_has_pass(x)





rcompare(TDAT , TDAT2 , outfile = "./testing2.txt")
