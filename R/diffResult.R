

diffResult <- R6::R6Class(
    classname = "diffResult",
    public = list(
        
        opts = NULL,
        checks = list(),
        result = NULL, 
        dfSummary = NULL,
        no_issues_found = NULL,
        
        initialize = function(base, comp, keys = NULL){
            
            self$no_issues_found = display$new(
                title = NULL,
                body = "No Issues Were Found"
            )
            
            if(is.null(keys)) keys <- "-- None Provided --"
            
            keysum <- data.frame(
                "Variable" = keys,
                stringsAsFactors = FALSE
            )
            
            class_base <- as_cropped_char( paste0(class(base), collapse = ", "))
            class_comp <- as_cropped_char( paste0(class(comp), collapse = ", "))
            
            dfsum <-  data.frame(
                "Summary" = c("Number of rows", "Number of columns", "Class"),
                "Base" = c( nrow(base), ncol(base), class_base),
                "Compare" = c( nrow(comp), ncol(comp), class_comp),
                stringsAsFactors = FALSE
            )
            
            self$dfSummary <- display$new(
                title = sprintf("Comparison of Base vs Compare"),
                body = list(
                    "Dataset Summary",
                    dfsum,
                    "",
                    "Listing of Keys",
                    keysum,
                    "",
                    "Differences were found, details are provided below",
                    ""
                )
            )
    
        },
        
        add_checkResult = function(CR){
            self$checks[[CR$name]] <- CR
        }
    )
)




#' print.diffResult
#' 
#' Print method for a diffdf result object (diffResult). Can be rendered
#' into different output formats and saved into a report file.
#' 
#' @param x diffResult Object
#' @param type Output format. Options = "ascii" (default) or "html"
#' @param rowlimit How many rows of a display dataset should be shown (default = 10)
#' @param file Location and name of a text file to output the results to. Setting to NULL will cause no file to be produced.
#' @param display Whether to print the rendered output in the R console
#' @param ... Additional arguments (not used)
#' 
#' @export
print.diffResult <- function(
    x, 
    type = "ascii", 
    rowlimit = 10, 
    file = NULL, 
    display = is.null(file), 
    ...
){
        
    stopifnot(
        any(type %in% c("ascii", "html"))
    )
    
    if( type == "ascii") render <- render_ascii
    if( type == "html") render <- render_html
    
    failed_displays <- Filter(
        function(x){ x$result == "Failed" },
        x$checks
    )
    
    failed_displays <- Map( 
        function(x) x$display,
        failed_displays
    )
    
    if(length(failed_displays) == 0){
        displays <- list(x$no_issues_found)
    } else {
        displays <- append(x$dfSummary, failed_displays)
    }
    
    strings_list <- lapply(
        displays,
        function(x) x$as.character(render, rowlimit = rowlimit)
    )
    
    strings = unlist(strings_list, use.names = FALSE)
    
    if(display) render$print(strings)
    if (!is.null(file)) render$file(file, strings)
    
    return(invisible(strings))
}




#' @export
#' @rdname print.diffResult
as.character.diffResult <- function(x, ...){
    x <- print(x, display = FALSE, ...)
    return(x)
}




#' summary.diffResult
#' 
#' Creates a summary of the issues found during the comparison
#' Returns a list of the issues in a format more accessible
#' for querying and exploring interactively
#' 
#' @param object A diffResult object created by diffdf()
#' @param ... Additional arguments (not used)
#' 
#' @export
summary.diffResult <- function(object, ...){

    x <- list()
    
    results <- data.table(
        Name = character(0),
        Result = character(0)
    )
    
    for( i in object$checks){
        
        if( i[["result"]] == "Failed"){
            x[[i[["name"]]]] <- i$data
        }
        
        results <- rbind(results, data.table(
            Name = i$name,
            Result = i$result
        ))
    }
    
    attr(x, "Results") <- results
    
    class(x) <- "diffSummary"
    return(x)
}



#' @export
print.diffSummary <- function(x, ...){
    print(attr(x, "Results"))
}

