
construct_s3 <- function(Class , ...){
    x <- list(...)
    class(x) <- Class
    return(x)
}


get_text <- function (object, ...) {
    UseMethod("get_text", object)
}

get_issue_message <- function (object, ...) {
    UseMethod("get_issue_message", object)
}

get_print_message <- function (object, ...) {
    UseMethod("get_print_message", object)
}

has_issue <- function (object, ...) {
    UseMethod("has_issue", object)
}


#' Title
#' 
#' Description
#' @param object TODO
has_issue.default <- function(object) TRUE




#' Title
#' 
#' Description
#' @param object TODO
get_print_message.default <- function(object){
    if( has_issue(object) ){  
        get_text(object)
    } else{
        NULL
    }
}


#' Title
#' 
#' Description
#' @param object TODO
get_issue_message.default <- function(object){
    if ( has_issue(object) ) {
        return( object$message)
    } else {
        return("")
    }
}

#' Title
#' 
#' Description
#' @param object TODO
#' @param dsin TODO
#' @param row_limit TODO
get_text.default <- function(object, dsin , row_limit = 10){
        
    if( nrow(dsin) == 0 ) {
        return("")
    }
    
    display_table <- dsin %>% 
        filter( row_number() < (row_limit + 1) )
    
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

has_issue.issue_basic <-  function(object){
    nrow(object$value) > 0
}


get_text.issue_basic <- function(object){
    NextMethod(dsin = object$value)
}


######  List of Issues

#' Title
#' 
#' Description
#' @param object TODO
#' @importFrom purrr map_lgl
has_issue.issue_list <- function(object){
    num_issues =  map_lgl( object$value , function(x) has_issue(x) ) %>% any
    return( num_issues )
}

#' Title
#' 
#' Description
#' @param object TODO
#' @importFrom purrr map
get_text.issue_list <- function(object){
    map( object$value , function(x) get_text(x))
}




###### Vector as an Issue


has_issue.issue_vector <-  function(object){
    sum(object$value) > 0
}


#' Title
#' 
#' Description
#' @param object TODO
#' @importFrom tibble rownames_to_column
get_text.issue_vector <- function(object){
    datin_tibble <- object$value %>% 
        as.data.frame() %>% 
        rownames_to_column()
    
    names(datin_tibble) <- c('Variable', 'No of Differences')
    
    datin_tibble <- datin_tibble %>% 
        filter(`No of Differences` > 0)
    
    NextMethod(dsin = datin_tibble)
}









