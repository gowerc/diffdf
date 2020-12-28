

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
            
            dfsum <-  data.frame(
                "Summary" = c("Number of rows", "Number of columns", "Class"),
                "BASE" = c( nrow(base), ncol(base), as_cropped_char(class(base))),
                "COMPARE" = c( nrow(comp), ncol(comp), as_cropped_char(class(comp))),
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
        },

        print = function(
            type = "ascii", 
            rowlimit = 10, 
            file = NULL,
            display = TRUE
        ){

            stopifnot(
                any(type %in% c("ascii", "html"))
            )
            
            if( type == "ascii"){
                as_title <- as_ascii_title
                as_string <- as_ascii_string
                as_table <- as_ascii_table
                render <- render_ascii
            }
            if( type == "html"){
                as_title <- as_html_title
                as_string <- as_html_string
                as_table <- as_html_table
                render <- render_html
            }
            
            failed_displays <- Filter(
                function(x){ x$result == "Failed" },
                self$checks
                
            )
            
            if(length(failed_displays) == 0){
                displays <- list(self$no_issues_found)
            } else {
                displays <- append(self$dfSummary, lapply(failed_displays, function(x) x$display))
            }
            
            strings_list <- lapply(
                displays,
                function(x){
                    x$as.character(
                        as_title = as_title,
                        as_string = as_string,
                        as_table = as_table,
                        rowlimit = rowlimit
                    )
                }
            )
                
            strings = unlist(strings_list, use.names = FALSE)
            
            if(display){
                render(strings)
            }
            
            if (!is.null(file)){
                tryCatch(
                    {
                        sink(file)
                        cat(strings, sep = "\n")
                        sink()
                    },
                    warning = function(w){
                        sink() 
                        warning(w)
                    },
                    error = function(e){
                        sink()
                        stop(e)
                    }
                )
            }
            
            return(invisible(strings))
        }
    )
)




#' @export
as.character.diffResult <- function(x, ...){
    x <- x$print(display = FALSE, ...)
    return(x)
}


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

