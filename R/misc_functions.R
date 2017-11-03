


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



#' class_adder
#' 
#' Adds a specific class to an object as well as adding any required
#' attribute data
#' @param objectin Object to add new class too
#' @param new_class New class to add to object
#' @param ...  Additional values (will be assigned as attributes)
class_adder <- function(objectin, new_class ,  ... ){
    ARGS <- list(...)
    class(objectin) <- append(new_class, class(objectin))
    for ( i in names(ARGS) ) {
        attr(objectin ,  i) <- ARGS[[i]] 
    }
    objectin
}




#' nonempty_list
#' 
#' <<TODO>>
#' @param in_list <<TODO>>
nonempty_list <- function(in_list){
    noenmpty_list <- in_list[map(in_list, nrow)>0]
    noenmpty_list
}



#' checklength
#' 
#' <<TODO>>
#' @param in_list <<TODO>>
checklength <- function(in_list){
    length(nonempty_list(in_list))
}







