render <- R6::R6Class(
    classname = "renderAbstract",
    public = list(
        
        strings = NULL,
        
        valid_content_types = c(
            "h1",
            "h2",
            "h3", 
            "h4",
            "p",
            "br",
            "table"
        ),
        
        initialize = function(content, rowlimit = 10){
            
            stopifnot(is.list(content))
            
            content_types = names(content)
            
            stopifnot(all(content_types %in% self$valid_content_types))
            
            strings = list()
            
            for( i in seq_along(content_types)){
                
                content_element <- content[[i]]
                content_type <- content_types[[i]]
                
                if(content_type == "table"){
                    limitstring <- NA
                    if( nrow(content_element) > rowlimit){
                        limitstring <- sprintf(
                            "Showing %i of %i observations",
                            rowlimit,
                            nrow(content_element)
                        )
                        content_element <- content_element[seq_len(rowlimit),]
                    }
                    string <- self$table(content_element, limitstring)
                } else {
                    string <- self[[content_type]](content_element)
                }
                
                strings[[i]] <- string
            }
            
            self$strings = unlist(strings, use.names = FALSE)
        },
        
        
        
        not_yet_implemented = function(name){
            stop(
                sprintf(
                    "The %s method has not yet been implemented for %s",
                    name,
                    getR6Class(self)
                )
            )
        },
        
        
        ## Content functions - Implemented in the individual classes
        h1 = function(x) self$not_yet_implemented("h1"),
        h2 = function(x) self$not_yet_implemented("h2"),
        h3 = function(x) self$not_yet_implemented("h3"),
        h4 = function(x) self$not_yet_implemented("h4"),
        p = function(x) self$not_yet_implemented("p"),
        br = function(x) self$not_yet_implemented("br"),
        table = function(x, limitstring) self$not_yet_implemented("table"),
        
        ## Output functions - Implemented in the individual classes
        display = function() self$not_yet_implemented("display"),
        file = function(filename) self$not_yet_implemented("file")
         
    )
)








