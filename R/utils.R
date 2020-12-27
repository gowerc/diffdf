remove_columns <- function(df, cols){
    keep_cols <- names(df)[!names(df) %in% cols]
    df2 <- df[, keep_cols, with = FALSE] 
    return(df2)
}

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