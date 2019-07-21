
#' string_pad
#'  
#' Utility function used to replicate str_pad. Adds white space to either end 
#' of a string to get it to equal the desired length
#' @param x string
#' @param width desired length 
string_pad <- function(x , width){
    if ( nchar(x) >= width) return(x)
    width <- width - nchar(x)
    left <- paste0( rep ( " " ,  floor( width/2) )  , collapse= "") 
    right <- paste0( rep ( " " ,  ceiling( width/2) )  , collapse= "") 
    paste0(  left , x ,right , collapse = "")
}


#' recursive_reduce
#'  
#' Utility function used to replicated purrr::reduce. Recursively applies a 
#' function to a list of elements until only 1 element remains
#' @param .l list of values to apply a function to
#' @param .f function to apply to each each element of the list in turn
#' i.e. .l[[1]] <- .f( .l[[1]] , .l[[2]]) ; .l[[1]] <- .f( .l[[1]] , .l[[3]])
recursive_reduce <- function(.l , .f){
    if (length(.l) != 1){
        .l[[2]] <- .f( .l[[1]] , .l[[2]])  
        return( recursive_reduce( .l[-1] , .f))
    } else {
        return(.l[[1]])
    }
}

#' invert
#'
#' Utility function used to replicated purrr::transpose. Turns a list inside
#' out. 
#' @param x list 
invert <- function(x){
    x2 <- list() 
    cnames <- names(x)
    tnames <- names(x[[1]])
    for ( i in tnames ){
        x2[[i]] <- list()
        for (j in cnames){
            x2[[i]][[j]] <- x[[j]][[i]]
        }
    }
    return(x2)
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