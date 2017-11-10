
#' Print rcompare objects
#' 
#' Print nicely formated version of an rcompare object
#' @param x comparison object created by rcompare().
#' @param VARIABLE specific variable to inspect the differences of (string).
#' @param ... Additional arguments (not used)
#' @examples
#' library(dplyr)
#' x <- iris %>% select( -Species)
#' x[1,2] <- 5
#' COMPARE <- rcompare( iris, x)
#' print( COMPARE )
#' print( COMPARE , "Sepal.Length" )
#' @importFrom purrr map_dbl
#' @importFrom purrr map
#' @export 
print.rcompare <- function(x, VARIABLE = NULL, ...){
    
    COMPARE <- x
    
    if ( !COMPARE$Issues){
        cat("No issues were found!")
        
    } else if ( !is.null(VARIABLE)) {
        
        outob <- COMPARE$VarDiffs$value[[VARIABLE]]$get_print_message()
        
        if(is.null(outob)){
            cat('Variable matched')
        } else {
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
        getorder <- map_dbl(COMPARE, function(x) x$order) %>% order
        COMPARE <- COMPARE[getorder]
        
        end_text <- map(COMPARE, function(x) x$get_print_message() ) %>% 
            unlist() %>% 
            paste(collapse = '')
        
        outtext <- paste0(start_text, end_text)
        cat(outtext)
    }
    
    invisible(COMPARE)
}




#' mod_stargazer
#'
#' simple modification to return only the object, not print as well!
#' @importFrom stargazer stargazer 
#' @importFrom  utils capture.output
#' @param ... Any arguments to give to stargazer
mod_stargazer <- function(...){
    paste0(
        "  " , 
        capture.output(stargazer(..., rownames = F))
    )
}




#' crop_char_value
#'
#' Makes any character string above x chars
#' Reduce down to a x char string with ...
#' @param inval a single element value
#' @param crop_at character limit
#' @importFrom stringr str_length
crop_char_value <- function(inval, crop_at = 30 ){

    if ( is.null(inval) ){
        
        inval <- "" 
        
    } else if ( is.na(inval)){
        
        inval <- ""
        
    } else {
        
        inval <- as.character(inval)
        
    }
    
    charlength <- str_length(inval)
    
    if (charlength > crop_at ){
        
        outval <- substr(inval, 1, crop_at )
        outval <- paste0(outval, '...')
        
    } else {
        
        outval <- inval
        
    }
    
    outval
}



