


checkResult <- R6::R6Class(
    classname = "checkResult",
    public = list(
        name = NULL,
        result = NULL,
        message = NULL, 
        data = NULL,
        display = NULL,
        exclude_rows = NULL,
        exclude_cols = NULL,
        
        initialize = function(
            name, 
            result, 
            message = NULL, 
            data = NULL, 
            display = NULL, 
            exclude_rows = NULL, 
            exclude_cols = NULL
        ){
            stopifnot(
                is.character(name),
                is.character(result),
                any(result %in% c("Passed", "Failed", "Not-Done")),
                ( is.null(message) | is.character(message)), 
                ( is.null(display) | all(class(display) == c("display", "R6")) ),
                ( is.null(exclude_rows) | is.list(exclude_rows)),
                ( is.null(exclude_cols) | is.character(exclude_cols))
                 
            )
            
            if( is.data.frame(data)){
                stopifnot(is.data.table(data))
                if( nrow(data) == 0){
                    data = NULL
                }
            }
            
            if( is.list(data)){
                if( length(data) == 0){
                    data = NULL
                }
            }
            
            if( is.character(exclude_cols)){
                if(length(exclude_cols) == 0){
                    exclude_cols <- NULL
                }
            }
            
            if( is.list(exclude_rows)){
                nam <- names(exclude_rows)
                nam_lgl <- nam %in% c("base", "comp")
                nam_extra <- nam[!nam_lgl]
                stopifnot( length(nam_extra) == 0)
                for(i in nam){
                    stopifnot(is.numeric(exclude_rows[[nam]]))
                    if( length(exclude_rows[[nam]]) == 0){
                        exclude_rows[[nam]] <- NULL
                    }
                }
                if(length(exclude_rows) == 0) exclude_rows <- NULL
            }
            
            self$name <- name
            self$result <- result
            self$message <- message
            self$data <- data
            self$display <- display
            self$exclude_rows <- exclude_rows
            self$exclude_cols <- exclude_cols
        }
        
    )
)



