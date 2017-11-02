



has_unique_rows <- function(DAT , KEYS){
    BYCHECK <- DAT %>%
        group_by_( .dots =  as.list(KEYS)  )  %>%
        summarise( ..n.. = n()) %>%
        filter( ..n.. > 1)
    
    return( nrow(BYCHECK) == 0 )
}



has_issues <- function(COMPARE){
    UseMethod("has_issues")
}


#'@export
has_issues.default <- function(COMPOB){
    if( attr(COMPOB, 'checkfun')(COMPOB)){
        attr(COMPOB, 'message')
    }else{
        ''
    }
}


#'@export
has_issues.rcompare_list <- function(COMPOB){
    ''
}


