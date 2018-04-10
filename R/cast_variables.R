
#' sort_join
#' 
#' Convenience function to sort two strings and paste them together
#' @param string1 first string
#' @param string2 second string
#' 
sort_join <- function(string1, string2){
    paste0(sort(c(string1, string2)), collapse = '')
}
#' class_merge
#' 
#' Convenience function to put all classes an object has into one string
#' @param x an object
class_merge <- function(x){
    paste(class(x), collapse = "_")
}

#' cast_vector
#' 
#' casts a vector depending on its type and input
#' @param colin column to cast
#' @param typecast type of casting to do
#' @param colname name of vector
#' @param whichdat whether base or compare is being casted (used for warnings)
#' 
cast_vector <- function(colin, typecast, colname,  whichdat){
    
    if (typecast == "characterfactor" & class(colin) == "factor"){
        warning(paste0("Casting ", colname, " in ", whichdat, " to character"))
        return(as.character(colin))
    }
    if (typecast == "integernumeric" & class(colin) == "integer"){
        warning(paste0("Casting ", colname, " in ", whichdat, " to numeric"))
        return(as.numeric(colin))
    }
    colin
}



#' cast_variables
#' 
#' Function to cast datasets columns if they have differting types
#' Restricted to specific cases
#' 
#' @param BASE base dataset
#' @param COMPARE comparison dataset
#' @param KEYS unique keys to compare with
#'
cast_variables <- function(BASE, COMPARE, keys){
    
    BASE_class <- data.frame(class_BASE = sapply(BASE, class_merge),
                             stringsAsFactors = FALSE)
    BASE_class <- rownames_to_column(BASE_class)
    
    COMPARE_class <- data.frame(class_COMPARE = sapply(COMPARE, class_merge),
                                stringsAsFactors = FALSE)
    COMPARE_class <- rownames_to_column(COMPARE_class)
    
    all_class <- merge(BASE_class, COMPARE_class, by = "rowname")
    all_class <- all_class[all_class$class_BASE != all_class$class_COMPARE,]

    all_class$classmerge <- mapply(sort_join, all_class$class_COMPARE, all_class$class_BASE)
    all_class <- all_class[all_class$classmerge %in% c("integernumeric", "characterfactor",
                                                       "characternumeric")]
    
    if(nrow(all_class)==0){
            return(list(BASE= BASE, COMPARE = COMPARE))
    }
    
    BASE[,all_class$rowname] <- mapply(cast_vector, 
                                      colin = as.list(BASE[,all_class$rowname] ),
                                      all_class$classmerge, 
                                      colname = names(BASE[,all_class$rowname]), 
                                      whichdat = "base",
                                      SIMPLIFY = FALSE)
    
    COMPARE[,all_class$rowname] <- mapply(cast_vector, 
                                       colin = as.list(COMPARE[,all_class$rowname] ),
                                       all_class$classmerge, 
                                       colname = names(COMPARE[,all_class$rowname]), 
                                       whichdat = "compare",
                                       SIMPLIFY = FALSE)
    
    return(list(BASE = BASE, COMP = COMPARE))
    
}