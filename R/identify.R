






identify_extra_rows <- function (DS1 , DS2 , KEYS){
    DS1 %>%
        anti_join( DS2 , by = KEYS) %>% 
        select_(.dots = list(KEYS)) 
}
# identify_extra_rows( TDAT , TDAT[1:11,] , "ID" )
# identify_extra_rows( TDAT[1:11,] , TDAT , "ID" )







identify_extra_cols <- function(DS1 , DS2){
    match.cols <- sapply ( names(DS1), "%in%", names(DS2))
    if (  !all(is.logical(match.cols)) ){
        stop("Assumption of logical return type is not true")
    }
    data_frame(
        COLUMNS = names(DS1)[ !match.cols]
    )
}
# identify_extra_cols( TDAT , TDAT[,1:6] )
# identify_extra_cols( TDAT[,1:6] , TDAT )







identify_matching_cols <- function(DS1, DS2 , EXCLUDE = ""){
    match_cols   <- sapply( names(DS1), "%in%" , names(DS2))
    exclude_cols <- sapply( names(DS1), "%in%" , EXCLUDE)
    names(DS1)[ match_cols & !exclude_cols ]
}
# identify_matching_cols( TDAT , TDAT[,1:6] )
# identify_matching_cols( TDAT[,1:6] , TDAT )
# identify_matching_cols( TDAT[,1:6] , TDAT , KEYS = c("GROUP1" ,"GROUP2"))
# identify_matching_cols( TDAT[,1] , TDAT[,1] , KEYS = "ID")





identify_unsupported_cols <- function(dsin){
    identify_properties(dsin) %>% 
        select( VARIABLE , MODE) %>% 
        filter( ! MODE %in%  c('numeric', 'character', 'logical') )
}





identify_mode_differences <- function( BASE, COMP ){
    
    matching_cols <- identify_matching_cols( BASE , COMP  )
    
    identify_properties(BASE) %>% 
        full_join(identify_properties(COMP) , by = "VARIABLE",  suffix = c(".BASE", ".COMP")) %>% 
        select( VARIABLE , MODE.BASE , MODE.COMP) %>% 
        filter( VARIABLE %in% matching_cols) %>% 
        filter( MODE.BASE != MODE.COMP)
}




identify_class_differences <- function( BASE, COMP ){
    
    matching_cols <- identify_matching_cols( BASE , COMP )
    
    identify_properties(BASE) %>% 
        full_join(identify_properties(COMP) , by = "VARIABLE",  suffix = c(".BASE", ".COMP")) %>% 
        select( VARIABLE , CLASS.BASE , CLASS.COMP) %>% 
        filter( VARIABLE %in% matching_cols) %>% 
        filter( !map2_lgl( CLASS.BASE , CLASS.COMP , identical)  )
}




identify_att_differences <- function( BASE, COMP , exclude_cols = "" ){

    matching_cols <- identify_matching_cols( BASE , COMP , exclude_cols )
    
    PROPS <- identify_properties(BASE) %>% 
        left_join(identify_properties(COMP) , by = "VARIABLE",  suffix = c(".BASE", ".COMP"))%>% 
        select( VARIABLE, ATTRIBS.BASE , ATTRIBS.COMP) %>% 
        filter( VARIABLE %in% matching_cols) 
    
    ### Setup dummy return value
    RETURN <- data_frame(
        VARIABLE = character(),
        ATTR_NAME = character(),
        VALUES.BASE = list(),
        VALUES.COMP = list()
    )
    
    for ( i in  PROPS$VARIABLE ){
        
        PROPS_filt <- PROPS %>% 
            filter( VARIABLE == i)
        
        ATT_DIFFS <- full_join(
            PROPS_filt$ATTRIBS.BASE[[1]] %>% identify_properties(),
            PROPS_filt$ATTRIBS.COMP[[1]] %>% identify_properties(),
            by = "VARIABLE", suffix = c(".BASE", ".COMP")
        ) %>% 
            rename( ATTR_NAME = VARIABLE) %>% 
            mutate( VARIABLE = i) %>% 
            filter( !map2_lgl( VALUES.BASE , VALUES.COMP , identical) ) %>% 
            select( VARIABLE , ATTR_NAME, VALUES.BASE , VALUES.COMP) 
        
        RETURN <- bind_rows(RETURN , ATT_DIFFS)
    }
    
    return(RETURN)
    
}






identify_differences <- function( BASE , COMP , KEYS, exclude_cols ) {
    
    matching_cols <- identify_matching_cols( BASE , COMP , c(KEYS, exclude_cols))
    
    if( length(matching_cols) == 0  ) return ( data_frame() )
    
    map(
        matching_cols,
        identify_variable_diff,
        KEYS = KEYS ,
        DAT = inner_join( BASE , COMP , by = KEYS , suffix = c(".x", ".y") )
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
        filter ( is_different(BASE , COMPARE)) %>% 
        as_data_frame()
    
}




identify_properties <- function(dsin){
    
    ### If missing or null return empty dataset
    if( is.null(dsin) ) {
        x <- data_frame(
            VARIABLE = character(),
            CLASS     = list(),
            MODE      = character(),
            TYPE      = character() ,
            ATTRIBS   = list(),
            VALUES    = list()
        )
        return(x)
    }
    
    data_frame(
        VARIABLE = names(dsin),
        CLASS     = map(dsin, class),
        MODE      = map_chr(dsin , mode),
        TYPE      = map_chr(dsin , typeof) ,
        ATTRIBS   = lapply( dsin , attributes),
        VALUES    = map( dsin , unlist )
    )
}


