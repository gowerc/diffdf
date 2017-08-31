




identify_extra_rows <- function (DS1 , DS2 , KEYS){
    DS1 %>%
        anti_join( DS2 , by = KEYS) %>%
        select_(.dots = list(KEYS))
}
# identify_extra_rows( TDAT , TDAT[1:11,] , "ID" )
# identify_extra_rows( TDAT[1:11,] , TDAT , "ID" )





identify_extra_cols <- function(DS1 , DS2){
    match.cols <- sapply ( names(DS1), "%in%", names(DS2))

    data_frame(
        COLUMNS = names(DS1)[ !match.cols]
    )

}
# identify_extra_cols( TDAT , TDAT[,1:6] )
# identify_extra_cols( TDAT[,1:6] , TDAT )





identify_matching_cols <- function(DS1, DS2 , KEYS = NA){
    match.cols <- sapply ( names(DS1), "%in%", names(DS2))
    matched <- names(DS1)[ match.cols]
    matched[!grepl(paste(KEYS, collapse = "|"), matched)]
}
# identify_matching_cols( TDAT , TDAT[,1:6] )
# identify_matching_cols( TDAT[,1:6] , TDAT )
# identify_matching_cols( TDAT[,1:6] , TDAT , KEYS = c("GROUP1" ,"GROUP2"))
# identify_matching_cols( TDAT[,1] , TDAT[,1] , KEYS = "ID")


identify_ilegal_cols<- function(indat){
  
  allowedtypes <- c('numeric', 'character', 'logical')
  datclass <- map_df(indat, mode)
  datfail <- datclass[!datclass %in% allowedtypes]
  return(datfail)
}


#pick out columns with different modes or if one is a factor and the other isn't

identify_mode_differences <- function( BASE, COMP , KEYS, exclude_cols){
  
  matching_cols <- identify_matching_cols( BASE , COMP , KEYS)
  
  matching_cols <- matching_cols[!matching_cols %in% exclude_cols]
  
  if( length(matching_cols) == 0  ) return ( data_frame() )
  
  modediffs(BASE, COMP, matching_cols)
  
  
}

identify_fact_level_differences <- function( BASE, COMP , KEYS, exclude_cols){
   
     matching_cols <- identify_matching_cols( BASE , COMP , KEYS)
     matching_cols <- matching_cols[!matching_cols %in% exclude_cols]
     
      if( length(matching_cols) == 0  ) return ( data_frame() )
      
     attdiffs(BASE, COMP, matching_cols, 'levels')
    
      
}

identify_label_differences <- function( BASE, COMP , KEYS, exclude_cols){
    
    matching_cols <- identify_matching_cols( BASE , COMP , KEYS)
    matching_cols <- matching_cols[!matching_cols %in% exclude_cols]
    
    if( length(matching_cols) == 0  ) return ( data_frame() )
    
    attdiffs(BASE, COMP, matching_cols, 'label')

}


identify_att_differences <- function( BASE, COMP , KEYS, exclude_cols){
  
  matching_cols <- identify_matching_cols( BASE , COMP , KEYS)
  matching_cols <- matching_cols[!matching_cols %in% exclude_cols]
  
  if( length(matching_cols) == 0  ) return ( data_frame() )
  
  att_diffs(BASE, COMP, matching_cols)
  
  
}



identify_differences <- function( BASE , COMP , KEYS, exclude_cols ) {

  matching_cols <- identify_matching_cols( BASE , COMP , KEYS)
  
  matching_cols <- matching_cols[!matching_cols %in% exclude_cols]

    if( length(matching_cols) == 0  ) return ( data_frame() )

    map(
        matching_cols,
        identify_variable_diff,
        KEYS = KEYS ,
        DAT = inner_join( BASE , COMP , by = KEYS )
    ) %>%
        set_names(matching_cols)
}
# identify_differences( TDAT, TDAT2, KEYS = "ID")
# identify_differences( TDAT, TDAT, KEYS = "ID")


identify_variable_diff <- function( VAR, DAT , KEYS){
  cname <- paste0(VAR , c(".x" , ".y"))
  
  DAT %>%
    select_( .dots =  c(KEYS , cname)) %>%
    rename_( .dots = set_names( as.list(cname),  c("BASE" , "COMPARE"))) %>%
    mutate( VARIABLE = VAR ) %>%
    select_(.dots =  c("VARIABLE" , KEYS , "BASE" , "COMPARE" )) %>%
    filter ( vectorcompare(BASE , COMPARE))
  
}




