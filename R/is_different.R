






#' is_different_wrapfun
#' 
#' Wrapper which passes is_different methods. Intended only for use
#' in is_different
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
is_different_wrapfun <- function (target, current, ...) {
    UseMethod("is_different")
}





#' is_different
#' 
#' This determines if two vectors are different. It expects vectors of the same
#' length and type, and is intended to be used after checks have already been done
#' Initially picks out any nas (matching nas count as a match)
#' Then compares remaining vector
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
#' @return A boolean vector which is T if target and current are different
is_different <- function (target, current, ...) {
    
    if( length(target) != length(current)){
        warning("Inputs are not of the same length")
        return(NULL)
    }
    
    if( is.null(target) | is.null(current) ){
        
        return( is.null(target) != is.null(current) )
        
    } else {
        
        N <- length(target)
        outvect <- rep(TRUE,N)
        
        
        nas_t <- is.na(target) 
        nas_c <- is.na(current)
        nacompare <- is.na(target) != is.na(current)
        
        selectvector <- as.logical( (!nas_t) * (!nas_c) )
        
        target  <- target[selectvector]
        current <- current[selectvector]  
        
        comparevect <- is_different_wrapfun(target,current)
        
        outvect[selectvector] <- comparevect
        
        outvect[nas_t|nas_c]  <- nacompare[nas_t|nas_c]
        
        as.logical(outvect)
        
        
    }  
}




#' is_different.default
#' 
#' Default method, if the vector is not numeric or factor. Basic comparison
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
is_different.default <- function(target, current, ...){
    target != current 
}




#' is_different.factor
#' 
#' Compares factors. Sets them as character and then compares
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
is_different.factor <- function(target, current, ...){
    as.character(target) != as.character(current) 
}





#' is_different.numeric
#' 
#' This is a modified version of the all.equal function
#' which returns a vector rather than a message
#' @param target the base vector
#' @param current a vector to compare target to
#' @param tolerance Level of tolerance for differences between two variables
#' @param scale Scale that tolerance should be set on. If NULL assume absolute
is_different.numeric <- function(
    target, 
    current, 
    tolerance = sqrt(.Machine$double.eps),
    scale = NULL
){
    if (!is.numeric(tolerance)) {
        stop("'tolerance' should be numeric")
    }
    
    if (!is.numeric(scale) && !is.null(scale)) {
        stop("'scale' should be numeric or NULL")
    }
    
    target <- as.vector(target)
    current <- as.vector(current)
    out <- target == current
    
    if (all(out)) {
        return(rep(FALSE, length(out)))
    }
    
    N <- length(target)
    target <- target[!out]
    current <- current[!out]
    
    if (is.integer(target) && is.integer(current)){ 
        target <- as.double(target)
    }
    
    xy <- mean(abs(target - current))
    
    what <- if (is.null(scale)) {
        xn <- mean(abs(target))
        if (is.finite(xn) && xn > tolerance) {
            xy <- xy/xn
        }
    } else {
        xy <- xy/scale
    }
    
    msg <- NULL
    
    if (is.na(xy) || xy > tolerance) {
        msg <- !out
    }
    
    if (is.null(msg)) {
        rep(FALSE,length(out))
    } else {
        msg
    }
    
}




