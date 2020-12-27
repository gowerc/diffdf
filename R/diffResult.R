

diffResult <- R6::R6Class(
    classname = "diffResult",
    public = list(
        opts = NULL,
        checks = list(),
        result = NULL, 
        dfSummary = NULL,
        initialize = function(){},
        add_checkResult = function(CR){
            self$checks[[CR$name]] <- CR
        }
    )
)

#' @export
summary.diffResult <- function(obj){

    x <- list()
    
    results <- tibble(
        Name = character(0),
        Result = character(0)
    )
    
    for( i in obj$checks){
        
        if( i[["result"]] == "Failed"){
            x[[i[["name"]]]] <- i$data
        }
        
        results <- rbind(results, tibble(
            Name = i$name,
            Result = i$result
        ))
    }
    
    x[["Results"]] <- results
    
    class(x) <- "diffSummary"
    return(x)
}


#' @export
print.diffSummary <- function(x){
    print(x$Results)
}

