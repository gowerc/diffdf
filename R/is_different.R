#' is_variable_different
#'
#' This subsets the data set on the variable name, picks out differences and returns a tibble
#' of differences for the given variable
#' @importFrom tibble as_tibble
#' @param variablename name of variable being compared
#' @param keynames name of keys
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
#' @return A boolean vector which is T if target and current are different
is_variable_different <- function (variablename, keynames, BASE, COMP,  ...) {
    
    
    if ( ! variablename %in% names(BASE) | ! variablename %in% names(COMP)){
        stop("Variable does not exist within input dataset")
    }
    
    target  <- BASE[[variablename]]
    current <- COMP[[variablename]]
    keys <- subset_se(BASE, cols = keynames)
    outvect <- find_difference(target, current, ...)
    x <- as_tibble(
        quickdf(
            c(
                list(VARIABLE = rep(variablename, sum(outvect))),
                subset(BASE, outvect, keynames),
                list(BASE = target[outvect],
                COMPARE = current[outvect])
            )
        )
    )
        
    
    return(x)
}

#' compare_vectors
#'
#' Compare two vectors looking for differences
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
compare_vectors <- function (target, current, ...) {
    UseMethod("compare_vectors")
}


#' find_difference
#'
#' This determines if two vectors are different. It expects vectors of the same
#' length and type, and is intended to be used after checks have already been done
#' Initially picks out any nas (matching nas count as a match)
#' Then compares remaining vector
#'
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
find_difference <- function (target, current,  ...) {
    
    if( length(target) != length(current)){
        warning("Inputs are not of the same length")
        return(NULL)
    }
    
    if( is.null(target) | is.null(current) ){
        return( is.null(target) != is.null(current) )
    }
    
    ### Initalise output, assume problem unless evidence otherwise
    
    
    comparevect <- compare_vectors(
        target ,
        current,
        ...
    )
    
    
    
    return(comparevect)
}







#' compare_vectors.default
#'
#' Default method, if the vector is not numeric, factor or character
#' Should not be called in practice, but there to handle exceptions
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
compare_vectors.default <- function(target, current, ...){
    
    return_vector <- rep(TRUE, length(target))
    nas_t <- is.na(target)
    nas_c <- is.na(current)
    
    ## compare missing values
    nacompare <- nas_t != nas_c
    naselect <- nas_t|nas_c
    return_vector[naselect]  <- nacompare[naselect]
    ## compare non-missing values
    selectvector <- as.logical( (!nas_t) * (!nas_c) )
    cvect <- target[selectvector] != current[selectvector]
    return_vector[selectvector] <- cvect
    return_vector
    
}




#' compare_vectors.factor
#'
#' Compares factors. Sets them as character and then compares
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
compare_vectors.factor <- function(target, current, ...){
    compare_vectors.character(as.character(target), as.character(current), ...)
}


#' compare_vectors.character
#'
#' Compares characters Sets them as character and then compares
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
compare_vectors.character<- function(target, current, ...){
        return(stringdiff(target, current))
}


#' compare_vectors.numeric
#'
#' This is a modified version of the all.equal function
#' which returns a vector rather than a message
#' @param target the base vector
#' @param current a vector to compare target to
#' @param tolerance Level of tolerance for differences between two variables
#' @param scale Scale that tolerance should be set on. If NULL assume absolute
compare_vectors.numeric <- function(
    target,
    current,
    tolerance = sqrt(.Machine$double.eps),
    scale = NULL
){
   if(is.null(scale)) scale <- 0
   return(doublediff(target, current, tolerance, scale))
}




