



check_unique_rows <- function(DAT , KEYS){
    BYCHECK <- DAT %>%
        group_by_( .dots =  as.list(KEYS)  )  %>%
        summarise( ..n.. = n()) %>%
        filter( ..n.. > 1)

    return( nrow(BYCHECK) == 0 )
}



check_is_equal <- function( VAL1 , VAL2){

    if ( is.null(VAL1) | is.null(VAL2) ) return (  is.null(VAL1) == is.null(VAL2))

    if ( is.na(VAL1) | is.na(VAL2) ) return (  is.na(VAL1) == is.na(VAL2))

    RES <- all.equal( VAL1 , VAL2 , tolerance = .Machine$double.eps^0.5)

    if ( RES != TRUE) return(FALSE)
    else return( TRUE)

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

    if ( nrow(COMPARE[["ExtColsBase"]])){
        if(!SUPWARN) warning("There are Columns in BASE that are not in COMPARE" )
        ISSUES <- TRUE
    }

    if ( nrow(COMPARE[["ExtColsComp"]])){
        if(!SUPWARN) warning("There are Columns in COMPARE that are not in BASE" )
        ISSUES <- TRUE
    }
    
    if ( nrow(COMPARE[["IllegalColsBase"]])){
      if(!SUPWARN) warning("There are Columns in BASE with unsupported modes" )
      ISSUES <- TRUE
    }
    
    if ( nrow(COMPARE[["IllegalColsCompare"]])){
      if(!SUPWARN) warning("There are Columns in COMPARE with unsupported modes" )
      ISSUES <- TRUE
    }

    if( sum(COMPARE[["NumDiff"]])){
        if(!SUPWARN) warning("Not all values compared equal")
        ISSUES <- TRUE
    }

    return(ISSUES)
}

