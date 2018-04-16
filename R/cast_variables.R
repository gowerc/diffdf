
#' sort_then_join
#' 
#' Convenience function to sort two strings and paste them together
#' @param string1 first string
#' @param string2 second string
sort_then_join <- function(string1, string2){
    paste0(sort(c(string1, string2)), collapse = '')
}


#' class_merge
#' 
#' Convenience function to put all classes an object has into one string
#' @param x an object
class_merge <- function(x){
    paste(class(x), collapse = "_")
}


get_message <- function( colname , whichdat, totype){
    message(paste0(
        "NOTE: Variable " , colname, " in " , tolower(whichdat) , " was casted to " , totype
    ))
}


#' get_casted_vector
#' 
#' casts a vector depending on its type and input
#' @param colin column to cast
#' @param colname name of vector
#' @param whichdat whether base or compare is being casted (used for messages)
get_casted_vector <- function(colin, colname,  whichdat){
    
    if ( class(colin) == "factor"){
        get_message( colname , whichdat , "character")
        return(as.character(colin))
    }
    
    if ( class(colin) == "integer"){
        get_message( colname , whichdat , "numeric")
        return(as.numeric(colin))
    }
    
    colin
}



#' get_casted_dataset
#' 
#' Internal utility function to loop across a dataset casting all target 
#' variables 
#' @param df dataset to be casted
#' @param columns columns to be casted
#' @param whichdat whether base or compare is being casted (used for messages)
get_casted_dataset <- function(df , columns , whichdat){
    for ( col in  columns ){
        df[[col]] <- get_casted_vector( df[[col]]  , col , whichdat )
    }
    return(df)
}






#' cast_variables
#' 
#' Function to cast datasets columns if they have differing types
#' Restricted to specific cases, currently integer and double, and character and factor
#' @param BASE base dataset
#' @param COMPARE comparison dataset
#' @param ignore_vars Variables not to be considered for casting
#' @param cast_integers Logical - Whether integers should be cased to double when compared to doubles
#' @param cast_factors Logical - Whether characters should be casted to characters when compared to characters
cast_variables <- function(
    BASE, 
    COMPARE, 
    ignore_vars = NULL, 
    cast_integers = FALSE , 
    cast_factors = FALSE 
){

    allowed_class_casts <- c( "integernumeric" , "characterfactor")[c(cast_integers, cast_factors)]
    
    BASE_class <- data.frame(
        class_BASE = sapply(BASE, class_merge), 
        colname = names(BASE),
        stringsAsFactors = FALSE
    )
    BASE_class <- BASE_class[!BASE_class[["colname"]] %in% ignore_vars,, drop=FALSE] 
    
    
    COMPARE_class <- data.frame(
        class_COMPARE = sapply(COMPARE, class_merge), 
        colname = names(COMPARE),
        stringsAsFactors = FALSE
    )
    COMPARE_class <- COMPARE_class[!COMPARE_class[["colname"]] %in% ignore_vars ,, drop=FALSE] 
    
    common_class <- merge(
        x = BASE_class, 
        y = COMPARE_class, 
        by = "colname"
    )
    
    
    diff_class <- common_class[ common_class[["class_BASE"]] != common_class[["class_COMPARE"]] ,,drop=FALSE]

    
    diff_class$classmerge <- mapply(
        sort_then_join, 
        diff_class$class_COMPARE, 
        diff_class$class_BASE
    )
    
    
    cast_columns <- diff_class[  diff_class[["classmerge"]] %in% allowed_class_casts ,,drop=FALSE] 
    
    
    DATASETS <- list(
        "BASE" = BASE,
        "COMPARE" = COMPARE
    )
    

    if( nrow(cast_columns) == 0 ){
        return( DATASETS )
    }
    
    
    for ( i in names(DATASETS)){
        DATASETS[[i]] <- get_casted_dataset( DATASETS[[i]] , cast_columns$colname , i)
    }
        
    
    return(DATASETS)
}
































