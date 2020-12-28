


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
            message, 
            data, 
            display = NULL, 
            exclude_rows = NULL, 
            exclude_cols = NULL
        ){
            stopifnot(
                any(result %in% c("Passed", "Failed", "Not-Done"))
            )
            
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



