



display <- R6::R6Class(
    classname = "display",
    public = list(
        title = NULL,
        body = NULL,
        
        initialize = function(title = NULL, body = NULL){
            self$title = title
            self$body = body
        },
        
        as.character = function(as_title, as_string, as_table, rowlimit){
  
            x <- list(
                title = as_title(self$title),
                body = lapply(
                    self$body, 
                    get_body, 
                    as_string = as_string, 
                    as_table = as_table, 
                    rowlimit = rowlimit
                )
            )
            
            strings <- unlist(
                list(
                    x$title,
                    "",
                    unlist(x$body, use.names = FALSE),
                    ""
                ), 
                use.names = FALSE
            )
            
            return(strings)
        }
    
    )
)

get_body <- function(x, as_string, as_table, rowlimit){
    
    if(is.character(x)) {
        y <- as_string(x)
    }
    
    if(is.data.frame(x)) {
        limitstring <- NA
        if( nrow(x) > rowlimit){
            limitstring <- sprintf(
                "Showing %i of %i observations",
                rowlimit,
                nrow(x)
                
            )
            x <- x[seq_len(rowlimit),]
        }
        y <- as_table(x, limitstring)
    }
    
    stopifnot(is.character(y))
    
    return(y)
}



