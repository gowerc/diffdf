


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








#' nonempty_list
#' 
#' <<TODO>>
#' @param in_list <<TODO>>
nonempty_list <- function(in_list){
    noenmpty_list <- in_list[map(in_list, nrow)>0]
    noenmpty_list
}


#' checklength
#' 
#' <<TODO>>
#' @param in_list <<TODO>>
checklength <- function(in_list){
    length(nonempty_list(in_list))
}



#' has_unique_rows
#' 
#' <<TODO>>
#' @param DAT <<TODO>>
#' @param KEYS <<TODO>>
#' @import dplyr
has_unique_rows <- function(DAT , KEYS){
    BYCHECK <- DAT %>%
        group_by_( .dots =  as.list(KEYS)  )  %>%
        summarise( ..n.. = n()) %>%
        filter( ..n.. > 1)
    
    return( nrow(BYCHECK) == 0 )
}

