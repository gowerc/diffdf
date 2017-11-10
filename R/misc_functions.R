


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
#' @import dplyr
has_unique_rows <- function(DAT , KEYS){
    BYCHECK <- DAT %>%
        group_by_( .dots =  as.list(KEYS)  )  %>%
        summarise( ..n.. = n()) %>%
        filter( ..n.. > 1)
    
    return( nrow(BYCHECK) == 0 )
}

