

context("Checking print function against gold standard")


# devtools::load_all()
# library(dplyr)
# library(stringr)
# library(devtools)
# library(testthat)
# source( "./tests/testthat/helper-create_test_data.R")





print_tests <- list(
    "Identical" = list(
        TDAT[ ,"ID", drop=FALSE], 
        TDAT2[ , "ID" , drop=FALSE]
    ), 
    
    "Identical 2" = list(  
        TDAT2[,"ID", drop=FALSE], 
        TDAT[, "ID" , drop =FALSE]
    ),
    
    
    
    "Different Values" = list(
        TDAT[, c("ID","CONTINUOUS")], 
        TDAT2[ ,c("ID","CONTINUOUS")]
    ), 
    
    "Different Values 2" = list(
        TDAT2[c("ID","CONTINUOUS")], 
        TDAT[c("ID","CONTINUOUS")]
    ),
    
    
    "Different attributes" = list(
        TDAT[,c("ID" , "BINARY")], 
        TDAT2[,c("ID" , "BINARY")]
    ),
    
    "Different attributes 2" = list(
        TDAT2[, c("ID" , "BINARY")], 
        TDAT[,c("ID" , "BINARY")]
    ),
    
    
    
    "Different Levels" = list(
        TDAT[,c("ID" , "CATEGORICAL")], 
        TDAT2[,c("ID" , "CATEGORICAL")]
    ),
    "Different Levels 2" = list(
        TDAT2[,c("ID" , "CATEGORICAL")], 
        TDAT[,c("ID" , "CATEGORICAL")]
    ),
    
    
    "Different Class" = list(
        TDAT[,c( "ID" , "DATE")], 
        TDAT2[,c( "ID" , "DATE")]
    ),
    "Different Class 2" = list(
        TDAT2[,c("ID" , "DATE")], 
        TDAT[,c("ID" , "DATE")]
    ),
    
    
    
    "Different Modes" = list(
        TDAT[,c( "ID" , "INTEGER")], 
        TDAT2[,c("ID" , "INTEGER")]
    ),
    "Different Modes 2" = list(
        TDAT2[,c( "ID" , "INTEGER")], 
        TDAT[,c("ID" , "INTEGER")]
    ),
    
    
    "Missing Columns" = list(
        TDAT[,c("ID" , "INTEGER", "BINARY")], 
        TDAT2[,c( "ID" , "INTEGER")]
    ),
    
    "Missing Columns 2" = list(
        TDAT2[,c( "ID" , "INTEGER", "BINARY")], 
        TDAT[,c("ID" , "INTEGER")]
    ),
    
    
    "Missing Rows" = list(
        TDAT[,c( "ID" , "GROUP1")]  , 
        TDAT2[ 1:nrow(TDAT2)<=10, c("ID" , "GROUP1")]
    ),
    "Missing Rows 2"  = list( 
        TDAT2[,c( "ID" , "GROUP1")]  , 
        TDAT[1:nrow(TDAT2) <= 10, c("ID" , "GROUP1")]
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
        {
            M_TDAT <- TDAT 
            M_TDAT$CHARACTER[1] <- NA
            M_TDAT
        },
        {
            M_TDAT2 <- TDAT 
            M_TDAT2$CHARACTER[1] <- ""
            M_TDAT2
        }
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
    for ( i in seq_along(RES) ){
        expect_equal( 
            RES[[i]] , 
            TESTING_print_msg[[i]] , 
            info = paste0( "Reference = " , i , " - " , names(RES)[i]))
    }
}

# i <- 6
# print_tests[[i]]
# RES[[i]]
# TESTING_print_msg[[i]]
# 
# rcompare(
#  print_tests[[i]][[1]],
#  print_tests[[i]][[2]]
# )

# for ( i in 1:length(RES)){
#     sink( paste0("./utils/print_output/output_",i,".txt"))
#     RES[[i]] %>% cat(sep = "\n")
#     sink()
# 
#     # sink( paste0("./utils/print_output/output_",i,"_gold.txt"))
#     # TESTING_print_msg[[i]] %>% cat(sep = "\n")
#     # sink()
# }



