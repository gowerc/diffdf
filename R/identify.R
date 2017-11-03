












#' identify_extra_rows
#' 
#' Identifys rows that are in a baseline dataset but not in a comparitor dataset
#' @param DS1 Baseline dataset (data frame)
#' @param DS2 Comparitor dataset (data frame)
#' @param KEYS List of variables that define a unique row within the datasets (strings)
#' @import dplyr
identify_extra_rows <- function (DS1 , DS2 , KEYS){
    DS1 %>%
        anti_join( DS2 , by = KEYS) %>% 
        select_(.dots = list(KEYS)) 
}







#' identify_extra_cols
#' 
#' Identifys columns that are in a baseline dataset but not in a comparitor dataset
#' @param DS1 Baseline dataset (data frame)
#' @param DS2 Comparitor dataset (data frame)
#' @import dplyr
identify_extra_cols <- function(DS1 , DS2){
    match.cols <- sapply ( names(DS1), "%in%", names(DS2))
    if (  !all(is.logical(match.cols)) ){
        stop("Assumption of logical return type is not true")
    }
    data_frame(
        COLUMNS = names(DS1)[ !match.cols]
    )
}







#' identify_matching_cols
#' 
#' Identifys columns with the same name in two data frames
#' @param DS1 Input dataset 1 (data frame)
#' @param DS2 Input dataset 2 (data frame)
#' @param EXCLUDE Columns to ignore
identify_matching_cols <- function(DS1, DS2 , EXCLUDE = ""){
    match_cols   <- sapply( names(DS1), "%in%" , names(DS2))
    exclude_cols <- sapply( names(DS1), "%in%" , EXCLUDE)
    names(DS1)[ match_cols & !exclude_cols ]
}





#' identify_unsupported_cols
#' 
#' Identifys any columns for which the package is not setup to handle
#' @param dsin input dataset
#' @import dplyr
identify_unsupported_cols <- function(dsin){
    identify_properties(dsin) %>% 
        select( VARIABLE , MODE) %>% 
        filter( ! MODE %in%  c('numeric', 'character', 'logical') )
}




#' identify_mode_differences
#' 
#' Identifys any mode differences between two data frames
#' @param BASE Base dataset for comparision (data.frame)
#' @param COMP Comparitor dataset to compare base against (data.frame)
#' @import dplyr
identify_mode_differences <- function( BASE, COMP ){
    
    matching_cols <- identify_matching_cols( BASE , COMP  )
    
    identify_properties(BASE) %>% 
        full_join(identify_properties(COMP) , by = "VARIABLE",  suffix = c(".BASE", ".COMP")) %>% 
        select( VARIABLE , MODE.BASE , MODE.COMP) %>% 
        filter( VARIABLE %in% matching_cols) %>% 
        filter( MODE.BASE != MODE.COMP)
}



#' identify_class_differences
#' 
#' Identifys any class differences between two data frames
#' @param BASE Base dataset for comparision (data.frame)
#' @param COMP Comparitor dataset to compare base against (data.frame)
#' @import dplyr
#' @importFrom purrr map2_lgl
identify_class_differences <- function( BASE, COMP ){
    
    matching_cols <- identify_matching_cols( BASE , COMP )
    
    identify_properties(BASE) %>% 
        full_join(identify_properties(COMP) , by = "VARIABLE",  suffix = c(".BASE", ".COMP")) %>% 
        select( VARIABLE , CLASS.BASE , CLASS.COMP) %>% 
        filter( VARIABLE %in% matching_cols) %>% 
        filter( !map2_lgl( CLASS.BASE , CLASS.COMP , identical)  )
}



#' identify_att_differences
#' 
#' Identifys any attribute differences between two data frames
#' @param BASE Base dataset for comparision (data.frame)
#' @param COMP Comparitor dataset to compare base against (data.frame)
#' @param exclude_cols Columns to exclude from comparision
#' @import dplyr
identify_att_differences <- function( BASE, COMP , exclude_cols = "" ){

    matching_cols <- identify_matching_cols( BASE , COMP , exclude_cols )
    
    PROPS <- identify_properties(BASE) %>% 
        full_join(identify_properties(COMP) , by = "VARIABLE",  suffix = c(".BASE", ".COMP")) %>% 
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
        
        ### Get a vector of all available attributes across both variables
        ATTRIB_NAMES = c( 
            names(PROPS_filt$ATTRIBS.BASE[[1]]) , 
            names(PROPS_filt$ATTRIBS.COMP[[1]])
        ) %>% unique
        
        ### If variable has no attributes move onto the next variable
        if ( is.null(ATTRIB_NAMES) ) next()
        
        ### Loop over each attribute checking if they are identical and outputing
        ### anyones that arn't
        for ( j in ATTRIB_NAMES){
            
            ATTRIB_BASE = PROPS_filt$ATTRIBS.BASE[[1]][j]
            ATTRIB_COMP = PROPS_filt$ATTRIBS.COMP[[1]][j]
            
            if ( !identical(ATTRIB_BASE , ATTRIB_COMP) ){
                
                ATT_DIFFS <- data_frame(
                    VARIABLE = i , 
                    ATTR_NAME = j , 
                    VALUES.BASE = ifelse( is.null(ATTRIB_BASE) , list() , ATTRIB_BASE),  
                    VALUES.COMP = ifelse( is.null(ATTRIB_COMP) , list() , ATTRIB_COMP)
                ) 
                
                RETURN <- bind_rows(RETURN , ATT_DIFFS)
            }
        }
    }
    return(RETURN)
}





#' identify_differences
#' 
#' Compares each column within 2 datasets to identify any values which they 
#' mismatch on.
#' @param BASE Base dataset for comparision (data.frame)
#' @param COMP Comparitor dataset to compare base against (data.frame)
#' @param KEYS List of variables that define a unique row within the datasets (strings)
#' @param exclude_cols Columns to exclude from comparision
#' @importFrom purrr pmap
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @import dplyr
identify_differences <- function( BASE , COMP , KEYS, exclude_cols ) {
    
    matching_cols <- identify_matching_cols( BASE , COMP , c(KEYS, exclude_cols))
    
    if( length(matching_cols) == 0  ) return ( data_frame() )
    
    outdat <- map(
        matching_cols,
        identify_variable_diff,
        KEYS = KEYS ,
        DAT = inner_join( BASE , COMP , by = KEYS , suffix = c(".x", ".y") )
    ) %>%
        set_names(matching_cols)
    
    outdat <- pmap(
        list(
            value = outdat,
            message = "" ,
            order = seq(1, length(outdat))
        ),
        issue_basic$new
    )
    outdat
}





#' identify_variable_diff
#' 
#' For a given variable find all values between 2 datasets where they mismatch.
#' The function takes only a single dataset as an input which is expected to have the relevent
#' columns from 2 input datasets already merged together. It is expected that the variable names
#' are differeniated by .x and .y
#' 
#' Function is only expected to be called from identify_differences which highly formats
#' the input prior to passing it to this function
#' @param VAR Variable to compare for differences (string)
#' @param DAT Input dataset (data_frame)
#' @param KEYS Variables which define a unique row within the dataset (strings)
#' @import dplyr
#' @importFrom  purrr set_names
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



#' identify_properties
#' 
#' Returns a dataframe of metadata for a given dataset.
#' Returned values include variable names , class , mode , type & attributes
#' @param dsin input dataframe that you want to get the metadata from
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @import dplyr
identify_properties <- function(dsin){
    
    ### If missing or null return empty dataset
    if( is.null(dsin) ) {
        x <- data_frame(
            VARIABLE = character(),
            CLASS     = list(),
            MODE      = character(),
            TYPE      = character() ,
            ATTRIBS   = list()
        )
        return(x)
    }
    
    data_frame(
        VARIABLE = names(dsin),
        CLASS     = map(dsin, class),
        MODE      = map_chr(dsin , mode),
        TYPE      = map_chr(dsin , typeof) ,
        ATTRIBS   = lapply( dsin , attributes)
    )
}






