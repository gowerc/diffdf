



display <- R6::R6Class(
    classname = "display",
    public = list(
        title = NULL,
        body = NULL,
        
        initialize = function(title = NULL, body = NULL){
            self$title = title
            self$body = body
        },
        
        as.character = function(render, rowlimit){
            
            x <- list(
                title = self$render_title(render),
                body =  self$render_body(render, rowlimit)
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
        },
        
        render_title = function(render){
            render$as_title(self$title)
        },
        
        render_body = function(render, rowlimit){
            x = lapply(self$body, self$render_body_element, render=render, rowlimit=rowlimit)
            return(x)
        },
        
        render_body_element = function(x, render, rowlimit){
            if(is.character(x)) {
                y <- render$as_string(x)
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
                y <- render$as_table(x, limitstring)
            }
            
            stopifnot(is.character(y))
            
            return(y)
        }
        
    )
)





