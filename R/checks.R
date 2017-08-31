



check_unique_rows <- function(DAT , KEYS){
    BYCHECK <- DAT %>%
        group_by_( .dots =  as.list(KEYS)  )  %>%
        summarise( ..n.. = n()) %>%
        filter( ..n.. > 1)

    return( nrow(BYCHECK) == 0 )
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
    
    if ( ncol(COMPARE[["IllegalColsBase"]])){
      if(!SUPWARN) warning("There are Columns in BASE with unsupported modes" )
      ISSUES <- TRUE
    }
    
    if ( ncol(COMPARE[["IllegalColsCompare"]])){
      if(!SUPWARN) warning("There are Columns in COMPARE with unsupported modes" )
      ISSUES <- TRUE
    }
    
    if ( nrow(COMPARE[["VarModeDiffs"]])){
      if(!SUPWARN) warning("There are Columns in BASE and COMPARE with different modes" )
      ISSUES <- TRUE
    }
    
    if ( nrow(COMPARE[["FactorlevelDiffs"]])){
      if(!SUPWARN) warning("There are Factor Columns in BASE and COMPARE with different levels" )
      ISSUES <- TRUE
    }
    
    if ( nrow(COMPARE[["AttribDiffs"]])){
      if(!SUPWARN) warning("There are columns in BASE and COMPARE with differing attributes" )
      ISSUES <- TRUE
    }

    if( sum(COMPARE[["NumDiff"]])){
        if(!SUPWARN) warning("Not all values compared equal")
        ISSUES <- TRUE
    }

    return(ISSUES)
}

