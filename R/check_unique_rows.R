



check_unique_rows <- function(DAT , KEY){
    BYCHECK <- DAT %>%
        group_by_( .dots =  KEY  )  %>%
        summarise( ..n.. = n()) %>%
        filter( ..n.. != 1)

    return( nrow(BYCHECK) == 0 )
}
