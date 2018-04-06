
# library(dplyr)
# library(testthat)


### Most common devtools functions
devtools::load_all()
devtools::test()
devtools::check()
devtools::document()

devtools::build()

### Ensure dfdiff is unloaded and removed
unloadNamespace("dfdiff")
utils::remove.packages("dfdiff")

### Document build and install R compare
devtools::document()
location <- devtools::build()
install.packages( location, repos = NULL, type="source")

### Very simple test
dfdiff::dfdiff(iris,iris)
library(dfdiff)
dfdiff(iris , iris)
?dfdiff

###############
#
#  Ensure R clinical is installed
#

library(rclinical)
?install

devtools::install_git( 
    "https://github.roche.com/Rpackages/dfdiff/tree/fix_dplyr_update", 
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
dfdiff(iris , iris)
dfdiff(TDAT , TDAT)
dfdiff(TDAT2 , TDAT2)

dfdiff(
    TDAT %>% select(ID , BINARY) ,
    TDAT2 %>% select( ID , BINARY) 
)

dfdiff(
    TDAT %>% select(ID , CATEGORICAL) ,
    TDAT2 %>% select( ID , CATEGORICAL) 
)

dfdiff(
    TDAT %>% select(ID , DATE) ,
    TDAT2 %>% select( ID , DATE) 
)

dfdiff(
    TDAT %>% select(ID , CONTINUOUS) ,
    TDAT2 %>% select( ID , CONTINUOUS) 
)

dfdiff(
    TDAT %>% select(ID , DATETIME) ,
    TDAT2 %>% select( ID , DATETIME) 
)

dfdiff(
    TDAT,
    TDAT2
)

TDAT3 <- TDAT %>% sample_frac(1)
dfdiff(TDAT , TDAT3)

x <- dfdiff(TDAT , TDAT2 , keys = c("ID" , "GROUP1"))
print(x ,VARIABLE =  "GROUP2")

class(x)


dfdiff_has_pass <- function(x){
    if (  class(x)[[1]] != "dfdiff" )  stop( "x is not an dfdiff object")
    return( length(x) == 0 ) 
}



dfdiff_has_pass(x)





dfdiff(TDAT , TDAT2 , outfile = "./testing2.txt")
