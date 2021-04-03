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
        
        initialize = function(display, rowlimit = 10){
            display_flat <- flatten_display(display)

            strings = list()
            
            for( i in seq_along(display_flat)){
                item_content <- display_flat[[i]]$content
                item_type <- display_flat[[i]]$type
                stopifnot(item_type %in% self$valid_content_types)
                if(item_type == "table"){
                    limitstring <- NA
                    if( nrow(item_content) > rowlimit){
                        limitstring <- sprintf(
                            "Showing %i of %i observations",
                            rowlimit,
                            nrow(item_content)
                        )
                        item_content <- item_content[seq_len(rowlimit),]
                    }
                    string <- self$table(item_content, limitstring)
                } else {
                    string <- self[[item_type]](item_content)
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




