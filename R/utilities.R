#' remove_columns
#' 
#' Removes named columns from a dataset
#' 
#' @param df dataset to remove columns from
#' @param cols columns to remove (character string)
remove_columns <- function(df, cols){
    keep_cols <- names(df)[!names(df) %in% cols]
    df2 <- df[, keep_cols, with = FALSE] 
    return(df2)
}

#' remove_columns
#' 
#' Removes rows from a dataset by index
#' 
#' @param df dataset to remove rows from
#' @param rows rows to remove (index) (numeric vector)
remove_rows <- function(df, rows){
    if(length(rows) == 0) return(df)
    df2 <- df[-rows,]
    return(df2)
}


#' get_properties
#' 
#' Returns a dataframe of metadata for a given dataset.
#' Returned values include variable names , class , mode , type & attributes
#' @param dsin input dataframe that you want to get the metadata from
get_properties <- function(dsin){
    
    ### If missing or null return empty dataset
    if(is.null(dsin)) {
        x <- data.table(
            VARIABLE = character(),
            CLASS     = list(),
            MODE      = character(),
            TYPE      = character(),
            ATTRIBS   = list()
        )
        return(x)
    }
    
    data.table(
        VARIABLE  = names(dsin),
        CLASS     = lapply(dsin, class),
        MODE      = sapply(dsin, mode),
        TYPE      = sapply(dsin, typeof),
        ATTRIBS   = lapply(dsin, attributes)
    )
}


#' as_cropped_char
#'
#' Makes any character string above x chars
#' Reduce down to a x char string with ...
#' @param inval a single element value
#' @param crop_at character limit
as_cropped_char <- function(inval, crop_at = 30 ){
    
    if ( is.null(inval) ){
        
        inval <- "<NULL>" 
        
    } else if ( is.na(inval)){
        
        inval <- "<NA>"
        
    } else {
        
        inval <- as.character(inval)
        
    }
    
    charlength <- sapply(inval, nchar)
    
    if (charlength > crop_at ){
        
        outval <- substr(inval, 1, crop_at )
        outval <- paste0(outval, '...')
        
    } else {
        
        outval <- inval
        
    }
    
    outval
}


#' totitle
#' 
#' Converts word to title case 
#' Poor mans version of str_to_title
#' 
#' @param x string to convert to title case
totitle <- function(x){
    firstup <- function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        x
    }
    x2 <- unlist(strsplit(x, " "))
    x3 <- firstup(x2)
    paste0(x3, collapse = " ")
}





