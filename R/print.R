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
print.diffdf <- function(x, ..., type = "html", raw = FALSE){
    outtext <- paste0(
        lapply(x, as.character, type = type ),
        collapse = ""
    )
    
    if(raw){
        return(outtext)
    } else {
        viewer(outtext, type = type)
        return(invisible(x))
    }
}





### TODO - Header "No issues were found!\n"
### TODO - Header 'Differences found between the objects!\n\n', 'A summary is given below.\n\n'
### TODO - As string for comparisons in unit tests

as.character.issue <- function(x, type = "html", ...){
    if(type == "html") return(render_html(x))
    if(type == "ascii") return(render_ascii(x))
}

viewer <- function(string, type){
    if(type == "html") return(html_viewer(string))
    if(type == "ascii") return(cat(string))
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
