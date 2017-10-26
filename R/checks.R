



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
has_issues.default <- function(COMPOB)
{
  if( attr(COMPOB, 'checkfun')(COMPOB)){
    attr(COMPOB, 'message')
  }else{
    ''
  }
}
#'@export
has_issues.rcompare_list <- function(COMPOB)
{
  ''
}


check_for_issues <- function(COMPARE , SUPWARN){
  
  getorder <- map_dbl(COMPARE, attr, 'order')
  COMPARE <- COMPARE[getorder]
  ISSUES <- map_chr(COMPARE, has_issues) %>% 
    paste(collapse ='')
  
  if( str_length(ISSUES) != 0 ){
    if(!SUPWARN) warning( c("\n" , ISSUES))
    return(TRUE)
  } else {
    return(FALSE)
  }
}

