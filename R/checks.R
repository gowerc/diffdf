



has_unique_rows <- function(DAT , KEYS){
    BYCHECK <- DAT %>%
        group_by_( .dots =  as.list(KEYS)  )  %>%
        summarise( ..n.. = n()) %>%
        filter( ..n.. > 1)
    
    return( nrow(BYCHECK) == 0 )
}

check_for_issues <- function(COMPARE , SUPWARN){

    ISSUES <- ""
    
    if( nrow(COMPARE[["ExtRowsBase"]]) ){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Rows in BASE that are not in COMPARE !!\n" 
        )
    }
    
    if( nrow(COMPARE[["ExtRowsComp"]]) ){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Rows in COMPARE that are not in BASE !!\n" 
        )
    }
    
    if ( nrow(COMPARE[["ExtColsBase"]])){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Columns in BASE that are not in COMPARE !!\n" 
        )
    }
    
    if ( nrow(COMPARE[["ExtColsComp"]])){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Columns in COMPARE that are not in BASE !!\n" 
        )
    }
    
    if ( COMPARE[["UnsupportedColsBase"]] %>%  nrow ){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Columns in BASE with unsupported modes !!\n" 
        )
    }
    
    if ( COMPARE[["UnsupportedColsComp"]] %>%  nrow ){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Columns in COMPARE with unsupported modes !!\n" 
        )
    }
    
    if ( nrow(COMPARE[["VarModeDiffs"]])){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Columns in BASE and COMPARE with different modes !!\n" 
        )
    }
    
    if ( nrow(COMPARE[["VarClassDiffs"]])){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Columns in BASE and COMPARE with different classes !!\n" 
        )
    }
    
    if ( nrow(COMPARE[["FactorlevelDiffs"]])){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Factor Columns in BASE and COMPARE with different levels !!\n" 
        )
    }
    
    if ( nrow(COMPARE[["LabelDiffs"]])){
        ISSUES <- paste0( 
            ISSUES, 
            "There are Columns in BASE and COMPARE with different labels !!\n" 
        )
    }
    
    if ( nrow(COMPARE[["AttribDiffs"]])){
        ISSUES <- paste0( 
            ISSUES, 
            "There are columns in BASE and COMPARE with differing attributes !!\n" 
        )
    }
    
    if( sum(COMPARE[["NumDiff"]])){
        ISSUES <- paste0( 
            ISSUES, 
            "Not all values compared equal\n"
        )
    }
    
    if( str_length(ISSUES) != 0 ){
        if(!SUPWARN) warning( c("\n" , ISSUES))
        return(TRUE)
    } else {
        return(FALSE)
    }
}

