
#' Print rcompare objects
#' 
#' Print nicely formated version of an rcompare object
#' @param x comparison object created by rcompare().
#' @param ... Additional arguments (not used)
#' @examples
#' x <- subset( iris , -Species )
#' x[1,2] <- 5
#' COMPARE <- rcompare( iris, x)
#' print( COMPARE )
#' print( COMPARE , "Sepal.Length" )
#' @export 
print.rcompare <- function(x, ...){
    COMPARE <- x

    if ( length(COMPARE) == 0 ){
        cat("No issues were found!")
        
    } else {
        
        start_text <- paste0(
            'Differences found between the objects!\n\n',
            'A summary is given below.\n\n'
        )

        end_text <- lapply(COMPARE, function(x) get_print_message(x) ) 
        end_text <- paste0(unlist(end_text), collapse = "")

        outtext <- paste0(start_text, end_text)
        cat(outtext)
    }
    
    invisible(COMPARE)
}


