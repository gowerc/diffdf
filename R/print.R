#' Print diffdf objects
#' 
#' Print nicely formatted version of an diffdf object
#' @param x comparison object created by diffdf().
#' @param ... Additional arguments (not used)
#' @param file Location and name of a text file to output the results to. Setting to NULL will cause no file to be produced.
#' @param as_string Return printed message as an R character vector? 
#' @examples
#' x <- subset( iris , -Species )
#' x[1,2] <- 5
#' COMPARE <- diffdf( iris, x)
#' print( COMPARE )
#' print( COMPARE , "Sepal.Length" )
#' @export 
print.diffdf <- function(x, ..., type = "html", raw = FALSE, file = NULL){
    
    outtext <- as.character(x, type = type)
    
    if (!is.null(file)){
        tryCatch(
            {
                sink(file)
                cat(outtext, sep = "\n")
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
    
    if(raw){
        return(outtext)
    } else {
        if(is.null(file)){
            display(outtext, type = type)
        }
        return(invisible(x))
    }
}


print.check <- function(x, ..., type = "html", raw = FALSE){
    outtext <- as.character(x, type = type)
    
    if(raw){
        return(outtext)
    } else {
        display(outtext, type = type)
        return(invisible(x))
    }
}


as.character.diffdf <- function(x, type = "html", ...){
    outtext <- paste0(
        lapply(x, as.character, type = type ),
        collapse = ""
    )  
    return(outtext)
}


### TODO - Header "No issues were found!\n"
### TODO - Header 'Differences found between the objects!\n\n', 'A summary is given below.\n\n'
### TODO - As string for comparisons in unit tests

as.character.check <- function(x, type = "html", ...){
    if(!get_print_me(x)) return("")
    if(type == "html") return(render_check_html(x))
    if(type == "ascii") return(render_check_ascii(x))
}



as.character.check_collection <- function(x, type = "html", ...){

    check_summary <- NextMethod()
    
    string <- paste0(
        sapply( get_collection(x), as.character, type = type),
        collapse = ""
    )
    
    paste0(
        check_summary,
        string,
        collapse = ""
    )
}

display <- function(string, type){
    if(type == "html") return(display_html(string))
    if(type == "ascii") return(display_ascii(string))
}



