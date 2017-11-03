


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


#' has_issues
#' 
#' <<TODO>>
#' @param OBJ <<TODO>>
has_issues <- function(OBJ){
    UseMethod("has_issues")
}


#' has_issues.default
#' 
#' <<TODO>>
#' @param OBJ <<TODO>>
has_issues.default <- function(OBJ){
    if( attr(OBJ, 'checkfun')(OBJ)){
        attr(OBJ, 'message')
    }else{
        ''
    }
}


#' has_issues.rcompare_list
#' 
#' <<TODO>>
#' @param OBJ <<TODO>>
has_issues.rcompare_list <- function(OBJ){
    ''
}


