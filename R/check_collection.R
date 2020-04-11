

construct_check_collection <- function(
    name = "", 
    value, 
    collection,
    is_test = TRUE,
    print_me = was_performed & !is_pass, 
    was_performed = TRUE,
    error_message = "", 
    is_pass = nrow(value) == 0, 
    ord = 999
){
   
    cls <- vapply( collection, class, character(1))
    stopifnot(
        length(collection) > 0,
        all(cls == "check")
    )
    
    x <- list(
        name = name,
        was_performed = was_performed,
        is_pass = is_pass, 
        is_test = is_test,
        value = value,
        print_me = print_me, 
        collection = collection, 
        error_message = error_message,
        ord = ord
    )
    class(x) <- c("check_collection" , "check")
    return(x)
}



get_collection.check_collection <- function(x, ...){
    return( x$collection)
}
