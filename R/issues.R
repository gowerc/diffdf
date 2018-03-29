#' construct_issue
#' 
#' Make an s3 object with class issue and possible additional class,
#' and assign other arguments to attributes
#' @param value the value of the object
#' @param message the value of the message attribute
#' @param order the value of the order attribute
#' @param add_class additional class to add
construct_issue <- function(value, message, order, add_class = NULL){
    x <- value
    class(x) <- c(add_class, "issue", class(x))
    attributes(x)[["message"]] <- message
    attributes(x)[["order"]]   <- order
    return(x)
}


#' get_table
#' 
#' Generate nice looking table from a data frame
#' @param dsin dataset 
#' @param row_limit Maximum number of rows displayed in dataset
get_table <- function(dsin , row_limit = 10){
    
    if( nrow(dsin) == 0 ) {
        return("")
    }
    
    display_table <- dsin %>% 
        subset( 1:nrow(dsin) < (row_limit + 1) )
    
    if ( nrow(dsin) > row_limit ){
        
        add_message <- paste0(
            'First ',
            row_limit, 
            " of " ,
            nrow(dsin),
            ' rows are shown in table below'
        )
        
    } else {
        add_message <- 'All rows are shown in table below'
    } 
    
    
    return(paste(c(add_message,
                   as_ascii_table(display_table),
                   '\n'),
                 collapse = '\n'))
}


#' get_issue_message
#' 
#' Simple function to grab the issue message
#' @param object inputted object of class issue
#' @param ... other arguments
get_issue_message <- function (object, ...) {
    if ( has_issue(object) ) {
        return( attr(object,"message"))
    } else {
        return("")
    }
}

#' get_print_message
#' 
#' Simple function to grab the print message
#' @param object inputted object of class issue
#' @param ... other arguments
get_print_message <- function (object, ...) {
    if( has_issue(object) ){  
        get_text(object)
    } else{
        NULL
    }
}


#' get_text
#' 
#' Get the required text depending on type of issue
#' @param object inputted object of class issue
#' @param ... other arguments
get_text <- function (object, ...) {
    UseMethod("get_text", object)
}







#' has_issue_message
#' 
#' Determine if a function has an issue
#' @param object inputted object of class issue
#' @param ... other arguments
has_issue <- function (object, ...) {
    UseMethod("has_issue", object)
}


#' has_issue.default
#' 
#' Default method for default objects, just returns true
#' @param object Inputted object
has_issue.default <- function(object) TRUE




#' get_text.default
#' 
#' Errors, as this should only ever be given an issue
#' @param object issue
get_text.default <- function(object) stop("Error: An issue has not been provided to this function!")




######  Basic issues 


#' has_issue.issue
#'
#'Check whether a basic issue has some problems by checking nrows in the object
#' @param object an object of class issue_basic
has_issue.issue <-  function(object){
    nrow(object) > 0
}

#' get_text.issue
#'
#' Get text from a basic issue, based on the class of the value of the issue
#'
#' @param object an object of class issue_basic
get_text.issue <- function(object){
    
    table_print <- get_table(object) 
    if (table_print == ""){
        return(table_print)
    }
    paste(
        c(attr(object, "message"), table_print),
        collapse = '\n'
    )
}


######  List of Issues

#' has_issue.issue_list
#' 
#' Determine if a list issue has any issues, looks through each object in turn and apply
#' has issue
#' @param object an issue_list object
#' @importFrom purrr map_lgl
has_issue.issue_list <- function(object){
    num_issues =  map_lgl( object , function(x) has_issue(x) ) %>% any
    return( num_issues )
}

#' get_text.issue_list
#' 
#' Get the text for a list object, by applying get_text over the list
#' @param object an object of class issue_list
#' @importFrom purrr map
get_text.issue_list <- function(object){
    map( object , function(x) get_text(x))
}












