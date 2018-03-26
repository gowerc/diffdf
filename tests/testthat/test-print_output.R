

context("Checking print function against gold standard")


# devtools::load_all()
# library(dplyr)
# library(stringr)
# library(devtools)
# library(testthat)
# source( "./tests/testthat/helper-create_test_data.R")




print_tests <- list(
    "Identical" = list(
        TDAT  %>% select( ID ), 
        TDAT2 %>% select( ID )
    ), 
    
    "Identical 2" = list(  
        TDAT2  %>% select( ID ), 
        TDAT %>% select( ID )
    ),
    
    
    
    "Different Values" = list(
        TDAT  %>% select( ID , CONTINUOUS), 
        TDAT2 %>% select( ID , CONTINUOUS)
    ), 
    
    "Different Values 2" = list(
        TDAT2  %>% select( ID , CONTINUOUS), 
        TDAT %>% select( ID , CONTINUOUS)
    ),
    
    
    "Different attributes" = list(
        TDAT  %>% select( ID , BINARY), 
        TDAT2 %>% select( ID , BINARY)
    ),
    
    "Different attributes 2" = list(
        TDAT2  %>% select( ID , BINARY), 
        TDAT %>% select( ID , BINARY)
    ),
    
    
    
    "Different Levels" = list(
        TDAT  %>% select( ID , CATEGORICAL), 
        TDAT2 %>% select( ID , CATEGORICAL)
    ),
    "Different Levels 2" = list(
        TDAT2  %>% select( ID , CATEGORICAL), 
        TDAT %>% select( ID , CATEGORICAL)
    ),
    
    
    "Different Class" = list(
        TDAT  %>% select( ID , DATE), 
        TDAT2 %>% select( ID , DATE)
    ),
    "Different Class 2" = list(
        TDAT2  %>% select( ID , DATE), 
        TDAT %>% select( ID , DATE)
    ),
    
    
    
    "Different Modes" = list(
        TDAT  %>% select( ID , INTEGER), 
        TDAT2 %>% select( ID , INTEGER)
    ),
    "Different Modes 2" = list(
        TDAT2  %>% select( ID , INTEGER), 
        TDAT %>% select( ID , INTEGER)
    ),
    
    
    "Missing Columns" = list(
        TDAT  %>% select( ID , INTEGER, BINARY), 
        TDAT2 %>% select( ID , INTEGER)
    ),
    
    "Missing Columns 2" = list(
        TDAT2  %>% select( ID , INTEGER, BINARY), 
        TDAT %>% select( ID , INTEGER)
    ),
    
    
    "Missing Rows" = list(
        TDAT  %>% select( ID , GROUP1)  , 
        TDAT2 %>% select( ID , GROUP1) %>% filter( row_number() <= 10),
        keys = "ID"
    ),
    "Missing Rows 2"  = list( 
        TDAT2  %>% select( ID , GROUP1)  , 
        TDAT %>% select( ID , GROUP1) %>% filter( row_number() <= 10),
        keys = "ID"
    ),
    
    
    "everything" = list(
        TDAT,
        TDAT2
    ),
    
    "everything 2"  = list(
        TDAT2,
        TDAT
    ),
    
    "Missing Vs NA"  = list(
        TDAT %>% mutate( CHARACTER= ifelse(row_number() ==1 , NA , CHARACTER)),
        TDAT %>% mutate( CHARACTER= ifelse(row_number() ==1 , "" , CHARACTER))
    )
)


runme <- function(x){
    utils::capture.output(rcompare(x[[1]] , x[[2]] , suppress_warnings = T))
}

RES <- map( print_tests , runme)

SET_GOLD <- FALSE

if ( SET_GOLD ){
    TESTING_print_msg <- RES
    devtools::use_data( TESTING_print_msg , internal = TRUE , overwrite = TRUE)
} else {
    for ( i in names(RES)){
        expect_equal( RES[[i]] , TESTING_print_msg[[i]] , info = paste0( "Reference = " , i))
    }
}


# for ( i in 1:length(RES)){
#     sink( paste0("./utils/print_output/output_",i,".txt"))
#     RES[[i]] %>% cat(sep = "\n")
#     sink()
# 
#     # sink( paste0("./utils/print_output/output_",i,"_gold.txt"))
#     # TESTING_print_msg[[i]] %>% cat(sep = "\n")
#     # sink()
# }



