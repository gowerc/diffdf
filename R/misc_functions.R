


#' factor_to_character
#' 
#' Takes a dataframe and converts any factor variables to character
#' @param dsin input dataframe 
#' @param vars variables to consider for conversion. Default NULL will consider 
#' every variable within the dataset
factor_to_character <- function( dsin , vars = NULL){
    
    if ( is.null(vars) ) vars = names(dsin)
    
    for (var in vars){
        if(  is.factor(dsin[[var]])){
            dsin[[var]] <- as.character(dsin[[var]])
        }
    }
    return(dsin)
}




#' has_unique_rows
#' 
#' Check if a data sets rows are unique
#' @param DAT input data set (data frame)
#' @param KEYS Set of keys which should be unique
has_unique_rows <- function(DAT , KEYS){
    DUPS <- duplicated( subset(DAT , select= KEYS) ) 
    NDUPS <- sum( DUPS)
    return( NDUPS == 0 )
}


#'convert_to_issue
#'
#'converts the count value into the correct issue format
#'@param datin data inputted
#'@importFrom tibble rownames_to_column
convert_to_issue <- function(datin){
    datin_tibble <- tibble(
        `Variable` = names(datin),
        `No of Differences` = datin
    )
    
    datin_tibble_reduced <- subset_se(
        df = datin_tibble, 
        rows = datin_tibble[["No of Differences"]] > 0
    )
    
    return(datin_tibble_reduced)
}



#' quickdf
#' 
#' Makes a data frame from a list very rapidly. Does not perform
#' Usual data.frame checks to allow for brevity
#' @param l a list wwhich follows data frame requirements exactly
quickdf <- function(l) {
    class(l) <- "data.frame"
    attr(l, "row.names") <- .set_row_names(length(l[[1]]))
    l
}



#' first_class
#' 
#' Convenience function to grab the first class message from a class call
#' 
#' @param col Variable we want to get the class from
first_class <- function(col){
    class(col)[1]
}



#' sort_df
#' 
#' Convenience function to sort a dataset (and hide the ugly syntax required to do so)
#' @param df dataframe to be sorted
#' @param by variables to sort the dataframe by (character vector)
sort_df <- function(df, by){
    df[do.call("order", df[by]), ]
}



#' subset_se
#' 
#' A version of subset that works only on standard evaluation (to avoid un-expected issues)
#' 
#' @param df dataframe to subset
#' @param rows row names / index / lgl vector to keep
#' @param cols column names / lgl vector to keep
subset_se <- function(df , rows = NULL, cols = NULL){
    
    if ( !(class(rows) %in% c("NULL", "logical", "numeric", "character")) | any(is.na(rows))){
        stop("Invalid rows specification")
    }
    
    if ( !(class(cols) %in% c("NULL", "logical", "character")) | any(is.na(cols))){
        stop("Invalid cols specification")
    }
    
    if( is.logical(rows)){
        if( ! length(rows) == nrow(df)){
            stop("If rows is logical it must be the same length as nrow(df)")
        }
    }
    
    if( is.logical(cols)){
        if( ! length(cols) == ncol(df)){
            stop("If cols is logical it must be the same length as ncol(df)")
        }
    }
    
    if( is.null(rows) & is.null(cols)) return(df)
    if( is.null(rows) ) return( df[ , cols , drop = FALSE] )
    if( is.null(cols) ) return( df[ rows , , drop = FALSE] )
    return( df[ rows , cols , drop = FALSE] )
}


#' get_column_mode
#' 
#' Convenience function to get the mode types of a dataframe.  
#'
#' @param  df dataframe to get column modes from
#' @param cols columns to subset. Column modes will be returned in the same order as specified by cols.  
#' If left as NULL it will return the model of all columns
get_column_mode <- function(df, cols = NULL){
    if( is.null(cols)) cols <- colnames(df)
    df2 <- subset_se(df, cols = cols)
    classtype <- vapply(df2 , mode, character(1))
    return(classtype)
}



