
#' Print diffdf objects
#' 
#' Print nicely formatted version of an diffdf object
#' @param x comparison object created by diffdf().
#' @param ... Additional arguments (not used)
#' @param as_string Return printed message as an R character vector? 
#' @examples
#' x <- subset( iris , -Species )
#' x[1,2] <- 5
#' COMPARE <- diffdf( iris, x)
#' print( COMPARE )
#' print( COMPARE , "Sepal.Length" )
#' @export 
print.diffdf <- function(x, ..., as_string = FALSE){
    COMPARE <- x

    if ( length(COMPARE) == 0 ){
        outtext <- "No issues were found!\n"
        
    } else {
        
        start_text <- paste0(
            'Differences found between the objects!\n\n',
            'A summary is given below.\n\n'
        )

        end_text <- lapply(COMPARE, function(x) get_print_message(x) ) 
        end_text <- paste0(unlist(end_text), collapse = "")

        outtext <- paste0(start_text, end_text)
    }
    
    if ( as_string){
        return(strsplit(outtext, '\n')[[1]])
    } else {
        cat(outtext)
        return(invisible(COMPARE))
    }
}



