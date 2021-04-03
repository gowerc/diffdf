

diffResult <- R6::R6Class(
    classname = "diffResult",
    public = list(
        
        opts = NULL,
        checks = list(),
        result = NULL, 
        meta = NULL,
        keys = NULL,
        call = NULL,
        check_results = NULL,
        
        initialize = function(base, comp, keys = NULL, call){
            
            self$keys = keys
            self$call = call
            self$meta = list(
                class_base = class(base),
                class_comp = class(comp),
                nrow_base = nrow(base),
                nrow_comp = nrow(comp),
                ncol_base = ncol(base),
                ncol_comp = ncol(comp)
            )        
            return(invisible(self))
        },
        
        add_checkResult = function(CR){
            stopifnot( "checkResult" %in% class(CR))
            self$checks[[CR$name]] <- CR
            return(invisible(self))
        },
        
        get_display_header = function(dfsummary = TRUE){
            
            if(self$result == "Passed"){
                line <- "All checks have passed"
            } else {
                line <- "Not all checks have passed, details are provided below"
            }
        
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
            
            if(dfsummary){
                header <- display(
                    d_h4("Dataset Summary"), 
                    d_table(dfsum), 
                    d_br(),
                    d_h4("Listing of Keys"), 
                    d_table(keysum), 
                    d_br(),
                    d_h4("Check Summary"), 
                    d_table(self$check_results), 
                    d_br()
                )
            } else {
                header <- list()
            }
            
            title <- display(
                d_h2(sprintf(
                    "Comparison of %s (Base) vs %s (Compare)", 
                    deparse(self$call$base), 
                    deparse(self$call$compare)
                )),
                d_br()
            )
            
            disp  = display(title, header)
            
            return(disp)
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
#' @param dfsummary Whether to print a summary of the two datasets in addition to check information
#' @param ... Additional arguments (not used)
#' 
#' @export
print.diffResult <- function(
    x, 
    dfsummary = TRUE,
    ...
){
    display <- extract_display(x, dfsummary=dfsummary)
    print(display, ...)
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
            
            k <- i$data
            
            ## If a dataset convert to tibble
            if( is.data.frame(k)) {
                k <- tibble::as_tibble(k)
            }
            
            ## If list of datasets loop through the list converting them to tibbles
            if( is.list(k)){
                for( data in names(k)){
                    k2 <- k[[data]]
                    if( is.data.frame(k2)){
                        k[[data]] <- tibble::as_tibble(k2)
                    }
                }
            }
            
            x[[i[["name"]]]] <- k
        }
    }
    attr(x, "Results") <- object$check_results
    class(x) <- "diffSummary"
    return(x)
}



#' @export
print.diffSummary <- function(x, ...){
    obj <- attr(x, "Results")
    render_ascii$print(
        render_ascii$as_table(obj)
    )
    return(invisible(tibble::as_tibble(obj)))
}


#' @export
extract_display <- function(x, dfsummary= TRUE){
    failed_checks <- Filter(
        function(x){ x$result == "Failed" },
        x$checks
    )
    
    failed_displays <- Map( 
        function(x) x$display,
        failed_checks
    )
    
    displays <- list()
    for(i in failed_checks){
        displays[[i$name]] <- i$display
    }
    
    
    displays <- append(
        list("summary" = x$get_display_header(dfsummary = dfsummary)), 
        displays
    )
    
    return(as_display(displays))
}
