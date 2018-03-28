#' construct_s3
#' 
#' Make an s3 object with added s3 class, preserving other arguments
#' @param Class new class to provide to bject
#' @param .... Other arguments
construct_s3 <- function(Class , ...){
    x <- list(...)
    class(x) <- Class
    return(x)
}

#' get_text
#' 
#' Get the required text depending on type of issue
#' @param object inputted object of class issue
#' @param ... other arguments
get_text <- function (object, ...) {
    UseMethod("get_text", object)
}

#' get_issue_message
#' 
#' Simple function to grab the issue message
#' @param object inputted object of class issue
#' @param ... other arguments
get_issue_message <- function (object, ...) {
    UseMethod("get_issue_message", object)
}


#' get_issue_message
#' 
#' Simple function to grab the print message
#' @param object inputted object of class issue
#' @param ... other arguments
get_print_message <- function (object, ...) {
    UseMethod("get_print_message", object)
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




#' get_print_message.default
#' 
#' Default method for geting print method. Checks if the object has an issue
#' And returns the text. Otherwise returns NULL
#' @param object inputted object
get_print_message.default <- function(object){
    if( has_issue(object) ){  
        get_text(object)
    } else{
        NULL
    }
}


#' get_issue_message.default
#' 
#' Default method for geting issue message. Checks if the object has an issue
#' And returns the message. Returns a blank otherwise
#' @param object inputted object
get_issue_message.default <- function(object){
    if ( has_issue(object) ) {
        return( object$message)
    } else {
        return("")
    }
}

#' get_text.default
#' 
#' Default method for getting text, generates ascii output for an issue
#' @param object issue
#' @param dsin dataset provided by issue
#' @param row_limit Maximum number of rows displayed in dataset
get_text.default <- function(object, dsin , row_limit = 10){
        
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
    
    RETURN <- paste(
        c(
            object$message,
            add_message,
            as_ascii_table(display_table),
            '\n'
        ),
        collapse = '\n'
    )
    
    return(RETURN)
}







######  Basic issues 
#' has_issue.issue_basic
#'
#'Check whether a basic issue has some problems by checking nrows in the object
#' @param object an object of class issue_basic
has_issue.issue_basic <-  function(object){
    nrow(object$value) > 0
}

#' get_text.issue_basic
#'
#' Get text from a basic issue, based on the class of the value of the issue
#'
#' @param object an object of class issue_basic
get_text.issue_basic <- function(object){
    NextMethod(dsin = object$value)
}


######  List of Issues

#' has_issue.issue_list
#' 
#' Determine if a list issue has any issues, looks through each object in turn and apply
#' has issue
#' @param object an issue_list object
#' @importFrom purrr map_lgl
has_issue.issue_list <- function(object){
    num_issues =  map_lgl( object$value , function(x) has_issue(x) ) %>% any
    return( num_issues )
}

#' get_text.issue_list
#' 
#' Get the text for a list object, by applying get_text over the list
#' @param object an object of class issue_list
#' @importFrom purrr map
get_text.issue_list <- function(object){
    map( object$value , function(x) get_text(x))
}




###### Vector as an Issue

#' has_issue.issue_vector
#' 
#' Determine if a vector issue has any issues, by summing over value
#' @param object an issue_vector object
has_issue.issue_vector <-  function(object){
    sum(object$value) > 0
}


#' get_text.issue_vector
#' 
#' Generate text for an issue_vector object, adds text at top then applies default method
#' @param object an issue_vector object
#' @importFrom tibble rownames_to_column
get_text.issue_vector <- function(object){
    datin_tibble <- object$value %>% 
        as.data.frame() %>% 
        rownames_to_column()
    
    names(datin_tibble) <- c('Variable', 'No of Differences')
    
    datin_tibble <- datin_tibble[ datin_tibble[["No of Differences"]] > 0, , drop = FALSE]
    
    NextMethod(dsin = datin_tibble)
}









