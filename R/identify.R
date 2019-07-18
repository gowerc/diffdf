

#' identify_extra_rows
#' 
#' Identifies rows that are in a baseline dataset but not in a comparator dataset
#' @param DS1 Baseline dataset (data frame)
#' @param DS2 Comparator dataset (data frame)
#' @param KEYS List of variables that define a unique row within the datasets (strings)
identify_extra_rows <- function(DS1, DS2 , KEYS){

    DS1in   <- sort_df(DS1, KEYS)   
    DS2in   <- sort_df(DS2, KEYS)   
    
    classtype <- get_column_mode(DS1in, cols = KEYS)
    
    index_select <- find_matches(
        subset_se( DS1in, cols = KEYS), 
        subset_se( DS2in, cols = KEYS),
        classtype, 
        length(KEYS)
    )
    #browser()
    index_select <- find_matches_3(
        subset_se( DS1in, cols = KEYS), 
        subset_se( DS2in, cols = KEYS)
    )
    
    x <- list(
        baseextra   = subset_se(DS1in, -index_select[[1]], KEYS),
        compextra   = subset_se(DS2in, -index_select[[2]], KEYS),
        base_reduce = subset_se(DS1in,  index_select[[1]]),
        comp_reduce = subset_se(DS2in,  index_select[[2]])
    )
    
    return(x)
}



# ds1 <- iris %>% sample_n(50)
# ds2 <- iris %>% sample_n(50)
# find_matches_3 <- function(ds1, ds2){
#     ds1$..ROWNUMBER1.. <- 1:nrow(ds1)
#     ds2$..ROWNUMBER2.. <- 1:nrow(ds2)
#     ds1 <- as.data.table(ds1)
#     ds2 <- as.data.table(ds2)
#     x <- merge(ds1 , ds2, sort= FALSE)
#     return(list(x$..ROWNUMBER1.. , x$..ROWNUMBER2..))
# }





#' identify_extra_cols
#' 
#' Identifies columns that are in a baseline dataset but not in a comparator dataset
#' @param DS1 Baseline dataset (data frame)
#' @param DS2 Comparator dataset (data frame)
#' @importFrom tibble tibble
identify_extra_cols <- function(DS1 , DS2){
    match.cols <- sapply ( names(DS1), "%in%", names(DS2))
    if (  !all(is.logical(match.cols)) ){
        stop("Assumption of logical return type is not true")
    }
    tibble(
        COLUMNS = names(DS1)[ !match.cols]
    )
}








#' identify_matching_cols
#' 
#' Identifies columns with the same name in two data frames
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
#' Identifies any columns for which the package is not setup to handle
#' @param dsin input dataset
identify_unsupported_cols <- function(dsin){
    dat <- subset_se( identify_properties(dsin) , cols = c("VARIABLE", "MODE") ) 
    
    subset_se(dat, rows = !dat[["MODE"]] %in% c('numeric', 'character', 'logical'))
}



#' identify_mode_differences
#' 
#' Identifies any mode differences between two data frames
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
identify_mode_differences <- function( BASE, COMP ){

    matching_cols <- identify_matching_cols( BASE , COMP  )
    
    dat <- merge(
        x = identify_properties(BASE),
        y = identify_properties(COMP),
        by = "VARIABLE",
        all = TRUE,
        suffixes = c(".BASE", ".COMP"),
        sort = TRUE
    ) 
    dat <-  subset_se( dat, cols =c("VARIABLE" , "MODE.BASE" , "MODE.COMP"))
    
    KEEP1 <- dat[["VARIABLE"]] %in% matching_cols
    KEEP2 <- dat[["MODE.BASE"]] != dat[["MODE.COMP"]]
    
    subset_se( dat , rows = KEEP1 & KEEP2)

}



#' identify_class_differences
#' 
#' Identifies any class differences between two data frames
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
identify_class_differences <- function( BASE, COMP ){
    
    matching_cols <- identify_matching_cols( BASE , COMP )
    
    dat <- merge(
        x = identify_properties(BASE),
        y = identify_properties(COMP),
        by = "VARIABLE",
        all = TRUE, 
        sort = TRUE,
        suffixes =  c(".BASE", ".COMP")
    ) 
    
    dat <- subset_se( dat , cols = c("VARIABLE" , "CLASS.BASE" , "CLASS.COMP")) 
    
    KEEP1 <- dat[["VARIABLE"]] %in% matching_cols
    KEEP2 <- !mapply( 
        identical,
        dat[["CLASS.BASE"]] , 
        dat[["CLASS.COMP"]] 
    )
    
    subset_se( dat , rows = KEEP1 & KEEP2)


}



#' identify_att_differences
#' 
#' Identifies any attribute differences between two data frames
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
#' @param exclude_cols Columns to exclude from comparison
#' @importFrom tibble tibble
identify_att_differences <- function( BASE, COMP , exclude_cols = "" ){
    
    matching_cols <- identify_matching_cols( BASE , COMP , exclude_cols )
    
    PROPS <- merge(
        x = identify_properties(BASE) ,
        y = identify_properties(COMP) , 
        by = "VARIABLE",  
        all = TRUE,
        sort = TRUE,
        suffixes = c(".BASE", ".COMP")
    )
    
    PROPS <- subset_se( 
        PROPS , 
        rows = PROPS[["VARIABLE"]] %in% matching_cols,
        cols =  c("VARIABLE", "ATTRIBS.BASE" , "ATTRIBS.COMP")
    ) 
 
    
    ### Setup dummy return value
    RETURN <- tibble(
        VARIABLE = character(),
        ATTR_NAME = character(),
        VALUES.BASE = list(),
        VALUES.COMP = list()
    )
    
    for ( i in  PROPS[["VARIABLE"]] ){
        
        PROPS_filt <- subset_se(PROPS, rows= PROPS[["VARIABLE"]] == i)

        ### Get a vector of all available attributes across both variables
        ATTRIB_NAMES = unique(c( 
            names(PROPS_filt[["ATTRIBS.BASE"]][[1]]) , 
            names(PROPS_filt[["ATTRIBS.COMP"]][[1]])
        ))
        
        ### If variable has no attributes move onto the next variable
        if ( is.null(ATTRIB_NAMES) ) next()
        
        ### Loop over each attribute checking if they are identical and outputing
        ### anyones that arn't
        for ( j in ATTRIB_NAMES){
            
            ATTRIB_BASE = PROPS_filt[["ATTRIBS.BASE"]][[1]][j]
            ATTRIB_COMP = PROPS_filt[["ATTRIBS.COMP"]][[1]][j]
            
            if ( !identical(ATTRIB_BASE , ATTRIB_COMP) ){
                
                ATT_DIFFS <- tibble(
                    VARIABLE = i , 
                    ATTR_NAME = j , 
                    VALUES.BASE = ifelse( is.null(ATTRIB_BASE) , list() , ATTRIB_BASE),  
                    VALUES.COMP = ifelse( is.null(ATTRIB_COMP) , list() , ATTRIB_COMP)
                ) 
                
                RETURN <- rbind(RETURN , ATT_DIFFS)
            }
        }
    }
    return(RETURN)
}





#' identify_differences
#' 
#' Compares each column within 2 datasets to identify any values which they 
#' mismatch on.
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
#' @param KEYS List of variables that define a unique row within the datasets (strings)
#' @param exclude_cols Columns to exclude from comparison
#' @param tolerance Level of tolerance for numeric differences between two variables
#' @param scale Scale that tolerance should be set on. If NULL assume absolute
identify_differences <- function( BASE, COMP, KEYS, exclude_cols,  
                                  tolerance = sqrt(.Machine$double.eps),
                                  scale = NULL) {
    
    matching_cols <- identify_matching_cols( BASE , COMP , c(KEYS, exclude_cols))
    
    if( length(matching_cols) == 0  ) return ( tibble() )
    
    matching_list <- mapply(
        is_variable_different , 
        matching_cols,
        MoreArgs = list(
            keynames = KEYS, 
            BASE = BASE,
            COMP = COMP, 
            tolerance = tolerance ,
            scale = scale
        ),
        SIMPLIFY = FALSE
    )
    
    matching_list
}









#' identify_properties
#' 
#' Returns a dataframe of metadata for a given dataset.
#' Returned values include variable names , class , mode , type & attributes
#' @param dsin input dataframe that you want to get the metadata from
#' @importFrom tibble tibble
identify_properties <- function(dsin){
    
    ### If missing or null return empty dataset
    if( is.null(dsin) ) {
        x <- tibble(
            VARIABLE = character(),
            CLASS     = list(),
            MODE      = character(),
            TYPE      = character() ,
            ATTRIBS   = list()
        )
        return(x)
    }
    
    tibble(
        VARIABLE = names(dsin),
        CLASS     = lapply(dsin, class),
        MODE      = sapply(dsin , mode),
        TYPE      = sapply(dsin , typeof) ,
        ATTRIBS   = lapply( dsin , attributes)
    )
}






