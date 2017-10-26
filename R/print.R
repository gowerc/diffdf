
#' Print nice formating of the rcompare object
#' @param x comparison object created by rcompare()
#' @param VARIABLE specific variable to inspect the differences of
#' @param ... Additional arguments (not used)
#' @examples
#' # x <- iris[150,1]
#' # COMPARE <- rcompare( iris, x)
#' # print( COMPARE )
#' # print( COMPARE , "Sepal.Length" )
#' @export 
print.rcompare <- function(x, VARIABLE = NULL, ...){
    
    COMPARE <- x
    
    if ( !COMPARE$Issues){
        cat("\n\n No issues were found!\n\n\n")
    }
    
    if ( !is.null(VARIABLE)) {
        
        print( COMPARE$VarDiffs[[VARIABLE]])
        
    } else {
        
        if ( COMPARE[["ExtColsBase"]] %>%  nrow ){
            cat("\n\nExtra Columns in Base that are not in Compare:\n")
            cat(COMPARE[["ExtColsBase"]]$COLUMNS , sep = ", ")
        }
        
        if ( COMPARE[["ExtColsComp"]] %>% nrow ){
            cat("\n\nExtra Columns in COMPARE that are not in BASE:\n")
            cat(COMPARE[["ExtColsComp"]]$COLUMNS , sep = ", ")
        }
        
        if ( COMPARE[["UnsupportedColsBase"]] %>%  nrow ){
            cat("\n\nColumns in BASE with unsupported modes:\n")
            cat(COMPARE[["UnsupportedColsBase"]]$VARIABLE , sep =  ", ")
        }
        
        if ( COMPARE[["UnsupportedColsComp"]] %>%  nrow ){
            cat("\n\nColumns in COMPARE with unsupported modes:\n")
            print(COMPARE[["UnsupportedColsComp"]])
        }
        
        if ( COMPARE[["VarModeDiffs"]] %>%  nrow ){
            cat("\n\nColumns in BASE and COMPARE with different modes:\n")
            print(COMPARE[["VarModeDiffs"]])
        }
        
        if ( COMPARE[["VarClassDiffs"]] %>%  nrow){
            cat("\n\nColumns in BASE and COMPARE with different classes:\n")
            print(COMPARE[["VarClassDiffs"]])
        }
        
        if ( COMPARE[["FactorlevelDiffs"]] %>%  nrow ){
            cat("\n\nFactor Columns in BASE and COMPARE with different levels:\n")
            print(COMPARE[["FactorlevelDiffs"]])
        }
        
        if ( COMPARE[["LabelDiffs"]] %>%  nrow ){
            cat("\n\nColumns in BASE and COMPARE with different labels:\n")
            print(COMPARE[["LabelDiffs"]])
        }
        
        if ( nrow(COMPARE[["AttribDiffs"]])){
            cat("\n\nColumns in BASE and COMPARE with differing attributes:\n")
            print(COMPARE[["AttribDiffs"]])
        }
        
        if( sum(COMPARE[["NumDiff"]])){
            cat("\n\nColumns with unequal values:\n")
            print(COMPARE[["NumDiff"]])
            cat("\nTo see specific descrepancies use print(<object>,<variable>)\n\n")
        }
    }

    invisible(COMPARE)
}





# x <- iris
# attr(x$Petal.Length, "rnd") <- "hi"
# attr(x$Petal.Length, "Label") <- "BLAH"
# x[150,1] <- 200
# x <- x[,-5]
# 
# COMPARE <- rcompare(iris , x)
# print(COMPARE)
# print(COMPARE , "Sepal.Length")
