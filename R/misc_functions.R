


#' factor_to_character
#' 
#' Takes a dataframe and converts any factor variables to character
#' @param dsin input dataframe 
#' @param vars variables to consider for conversion. Default NULL will consider 
#' every variable within the dataset
factor_to_character <- function( dsin , vars = NULL){
    
    if ( is.null(vars) ) vars = names(dsin)
    
    for (var in vars){
        if(  is.factor(dsin[[var]])){
            dsin[[var]] <- as.character(dsin[[var]])
        }
    }
    return(dsin)
}




#' has_unique_rows
#' 
#' Check if a data sets rows are unique
#' @param DAT input data set (data frame)
#' @param KEYS Set of keys which should be unique
has_unique_rows <- function(DAT , KEYS){
    DUPS <- duplicated( subset(DAT , select= KEYS) ) 
    NDUPS <- sum( DUPS)
    return( NDUPS == 0 )
}

#'convert_to_issue
#'
#'converts the count value into the correct issue format
#'@param datin data inputted
#'@importFrom tibble rownames_to_column
convert_to_issue <- function(datin){
    datin_tibble <- tibble(
        `Variable` = names(datin),
        `No of Differences` = datin
    )
    
    datin_tibble_reduced <- datin_tibble[ datin_tibble[["No of Differences"]] > 0, , drop = FALSE]
    return(datin_tibble_reduced)
}

#' quickdf
#' 
#' Makes a data frame from a list very rapidly. Does not perform
#' Usual data.frame checks to allow for brevity
#' @param l a list wwhich follows data frame requirements exactly
quickdf <- function(l) {
    class(l) <- "data.frame"
    attr(l, "row.names") <- .set_row_names(length(l[[1]]))
    l
}

#' first_class
#' 
#' Convenience function to grab the first class message from a class call
#' 
#' @param col Variable we want to get the class from
first_class <- function(col){
    class(col)[1]
}
