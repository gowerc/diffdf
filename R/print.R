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


#' get_table
#' 
#' Generate nice looking table from a data frame
#' @param dsin dataset 
#' @param row_limit Maximum number of rows displayed in dataset
get_table <- function(dsin , row_limit = 10){
    
    if( nrow(dsin) == 0 ) {
        return("")
    }
    display_table <- dsin[ 1:nrow(dsin) < (row_limit + 1), ]
    
    if ( nrow(dsin) > row_limit ){
        
        add_message <- paste0(
            'First ',
            row_limit, 
            " of " ,
            nrow(dsin),
            ' rows are shown in table below'
        )
        
    } else {
        add_message <- 'All rows are shown in table below'
    } 
    
    msg <- paste(
        c(
            add_message,
            render_ascii(display_table),
            '\n'
        ),
        collapse = '\n'
    )
    
    return(msg)
}
