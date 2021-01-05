

diffResult <- R6::R6Class(
    classname = "diffResult",
    public = list(
        
        opts = NULL,
        checks = list(),
        result = NULL, 
        meta = NULL,
        check_summary = NULL,
        summary = NULL,
        keys = NULL,
        
        initialize = function(base, comp, keys = NULL){
            
            self$keys = keys
            
            self$meta = list(
                class_base = class(base),
                class_comp = class(comp),
                nrow_base = nrow(base),
                nrow_comp = nrow(comp),
                ncol_base = ncol(base),
                ncol_comp = ncol(comp)
            )
        },
        
        add_checkResult = function(CR){
            self$checks[[CR$name]] <- CR
        },
        
        generate_summary = function(){
            
            if(is.null(self$keys)){
                keys <- "  None Provided  "
            } else {
                keys <- self$keys
            }
            
            keysum <- data.frame(
                "Variable" = keys,
                stringsAsFactors = FALSE
            )
            
            class_base <- as_cropped_char( paste0(self$meta$class_base, collapse = ", "))
            class_comp <- as_cropped_char( paste0(self$meta$class_comp, collapse = ", "))
            
            
            dfsum <-  data.frame(
                "Summary" = c("Number of rows", "Number of columns", "Class"),
                "Base" = c( self$meta$nrow_base, self$meta$ncol_base, class_base),
                "Compare" = c( self$meta$nrow_comp, self$meta$ncol_comp, class_comp),
                stringsAsFactors = FALSE
            )
            
            results <- data.table(
                Name = character(0),
                Result = character(0)
            )
            for( i in self$checks){
                results <- rbind(results, data.table(
                    Name = i$name,
                    Result = i$result
                ))
            }
            
            self$check_summary = results
            
            if(self$result == "Passed"){
                line <- "All checks have passed"
            } else {
                line <- "Not all checks have passed, details are provided below"
            }
            
            self$summary <- display$new(
                title = sprintf("Comparison of Base vs Compare"),
                body = list(
                    "Dataset Summary",
                    dfsum,
                    "",
                    "Listing of Keys",
                    keysum,
                    "",
                    "Check Summary",
                    results,
                    "",
                    line,
                    ""
                )
            )
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
    
    displays <- append(x$summary, failed_displays)
    
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
    for( i in object$checks){
        if( i[["result"]] == "Failed"){
            x[[i[["name"]]]] <- i$data
        }
    }
    attr(x, "Results") <- object$check_summary
    class(x) <- "diffSummary"
    return(x)
}



#' @export
print.diffSummary <- function(x, ...){
    print(attr(x, "Results"))
}

