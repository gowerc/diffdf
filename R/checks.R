



check_unique_rows <- function(DAT , KEY){
    BYCHECK <- DAT %>%
        group_by_( .dots =  as.list(KEY)  )  %>%
        summarise( ..n.. = n()) %>%
        filter( ..n.. > 1)

    return( nrow(BYCHECK) == 0 )
}

check_is_equal <- function( VAL1 , VAL2){
    VAL1 != VAL2 |
        ( is.na(VAL1) != is.na(VAL2))  |
        (is.null(VAL1) != is.null(VAL2))
}


check_for_issues <- function(COMPARE , SUPWARN){

    ISSUES <- FALSE

    if( nrow(COMPARE[["ExtRowsBase"]]) ){
        if(!SUPWARN) warning("There are Rows in BASE that are not in COMPARE" )
        ISSUES <- TRUE
    }

    if( nrow(COMPARE[["ExtRowsComp"]]) ){
        if(!SUPWARN) warning("There are Rows in COMPARE that are not in BASE" )
        ISSUES <- TRUE
    }

    if ( length(COMPARE[["ExtColsBase"]])){
        if(!SUPWARN) warning("There are Columns in BASE that are not in COMPARE" )
        ISSUES <- TRUE
    }

    if ( length(COMPARE[["ExtColsComp"]])){
        if(!SUPWARN) warning("There are Columns in COMPARE that are not in BASE" )
        ISSUES <- TRUE
    }

    if( sum(COMPARE[["NumDiff"]])){
        if(!SUPWARN) warning("Not all values compared equal")
        ISSUES <- TRUE
    }

    return(ISSUES)
}

