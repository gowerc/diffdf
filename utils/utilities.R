
# library(dplyr)
# library(testthat)


### Most common devtools functions
devtools::load_all()
devtools::test()
devtools::check()
devtools::document()

devtools::build()

### Ensure diffdf is unloaded and removed
unloadNamespace("diffdf")
utils::remove.packages("diffdf")

### Document build and install R compare
devtools::document()
location <- devtools::build()
install.packages( location, repos = NULL, type="source")

### Very simple test
diffdf::diffdf(iris,iris)
library(diffdf)
diffdf(iris , iris)
?diffdf

###############
#
#  Ensure R clinical is installed
#

library(rclinical)
?install

devtools::install_git( 
    "https://github.roche.com/Rpackages/diffdf/tree/fix_dplyr_update", 
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
diffdf(iris , iris)
diffdf(TDAT , TDAT)
diffdf(TDAT2 , TDAT2)


library(dplyr)
diffdf(
    TDAT %>% select(ID , INTEGER) %>% mutate( INTEGER = as.numeric(INTEGER)) ,
    TDAT %>% select( ID , INTEGER) , 
    strict_numeric = FALSE
)

diffdf(
    TDAT %>% select(ID , CATEGORICAL) ,
    TDAT2 %>% select( ID , CATEGORICAL) 
)

diffdf(
    TDAT %>% select(ID , DATE) ,
    TDAT2 %>% select( ID , DATE) 
)

diffdf(
    TDAT %>% select(ID , CONTINUOUS) ,
    TDAT2 %>% select( ID , CONTINUOUS) 
)

diffdf(
    TDAT %>% select(ID , DATETIME) ,
    TDAT2 %>% select( ID , DATETIME) 
)

diffdf(
    TDAT,
    TDAT2
)

TDAT3 <- TDAT %>% sample_frac(1)
diffdf(TDAT , TDAT3)

x <- diffdf(TDAT , TDAT2 , keys = c("ID" , "GROUP1"))
print(x ,VARIABLE =  "GROUP2")

class(x)


diffdf_has_pass <- function(x){
    if (  class(x)[[1]] != "diffdf" )  stop( "x is not an diffdf object")
    return( length(x) == 0 ) 
}



diffdf_has_pass(x)





diffdf(TDAT , TDAT2 , outfile = "./testing2.txt")
