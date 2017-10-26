
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
    
  }else if ( !is.null(VARIABLE)) {
    outob <- make_textout( COMPARE$VarDiffs[[VARIABLE]],
                           row_limit = 100)
    if(is.null(outob)){
      cat('Variable matched')
    }else{
    cat(outob)
    }
    
  } else {
    
    start_text <- paste0(
      'Differences found between the objects!\n\n',
      'A summary is given below.\n\n',
      'Please use print(, Variable = "Name") to examine in more, ',
      'detail where necessary.\n\n'
    )
    
    #Start by looking at simple comparisons
    #extra columns/rows and illegal columns
    #We make a set of 7 arguments to pass to pastefun, defined above
    COMPARE$Issues <- NULL
    getorder <- map_dbl(COMPARE, attr, 'order')
    COMPARE <- COMPARE[getorder]
    
    end_text <- map(COMPARE, make_textout) %>% 
      unlist() %>% 
      paste(collapse = '')
    outtext <- paste0(start_text, end_text)
    cat(outtext)
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
