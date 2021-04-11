render <- R6::R6Class(
    classname = "renderAbstract",
    public = list(
        
        strings = NULL,
        
        valid_content_types = c(
        ),
        
        initialize = function(display, rowlimit = 10){
            display_flat <- flatten_display(display)

            strings = list()
            
            for( i in seq_along(display_flat)){
                item_content <- display_flat[[i]]$content
                item_type <- self$class_handle(display_flat[[i]]$type)
                if(item_type == "table"){
                    string <- self$table_handle(item_content, rowlimit)
                } else {
                    string <- self[[item_type]](item_content)
                }
                
                strings[[i]] <- string
            }
            
            self$strings = unlist(strings, use.names = FALSE)
        },
        
        class_handle = function(class){
            if(class %in% self$valid_content_types){
                return(class)
            }else{
                return(not_yet_implemented)
            }
        },
        
        table_handle = function(item_content, rowlimit){
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
        },
        
        
        
        not_yet_implemented = function(name){
            stop(
                sprintf(
                    "The %s method has not yet been implemented for %s",
                    name,
                    getR6Class(self)
                )
            )
        }
        
         
    )
)




