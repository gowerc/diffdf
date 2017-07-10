

identify_extra_rows <- function (DS1 , DS2 , KEY){
    DS1 %>%
        anti_join( DS2 , by = KEY) %>%
        select_(.dots = list(KEY))
}

identify_matching_rows <- function (DS1 , DS2 , KEY){
    DS1 %>%
        inner_join( DS2 , by = KEY)
}


identify_extra_cols <- function(DS1 , DS2){
    match.cols <- sapply ( names(DS1), "%in%", names(DS2))
    names(DS1)[ !match.cols]
}

identify_matching_cols <- function(DS1, DS2 , KEY = NA){
    match.cols <- sapply ( names(DS1), "%in%", names(DS2))
    matched <- names(DS1)[ match.cols]
    matched[!grepl(paste(KEY, collapse = "|"), matched)]

}

identify_value_diffs <- function( DS1 , DS2 , KEY , VAR){
    cname <- paste0(VAR , c(".x" , ".y"))

    identify_matching_rows(DS1 , DS2, KEY) %>%
        select_ ( .dots =  c(KEY , cname)) %>%
        rename_( .dots = setNames( as.list(cname),  c("BASE" , "COMPARE"))) %>%
        mutate( VARIABLE = VAR ) %>%
        select_(.dots =  c("VARIABLE" , KEY , "BASE" , "COMPARE" )) %>%
        filter ( check_is_equal(BASE , COMPARE))
}



