check_values <- function(base, comp, keys, opts) {
    
    compare_cols <- names(base)[!names(base) %in% keys]
    
    if( length(compare_cols) == 0  ) {
        x <- checkResult$new(
            name = "Values",
            display = display$new(), 
            result = "Passed", 
            message = "Not all Values Compared Equal", 
            data = tibble()
        )
        
        return(x)
    }
    
    DAT <- merge(
        base, 
        comp, 
        by = keys, 
        suffixes = c(".BASE", ".COMPARE")
    )   

    matching_list <- mapply(
        is_variable_different , 
        compare_cols,
        MoreArgs = list(
            keynames = keys, 
            DAT = DAT,
            tolerance = opts$tolerance,
            scale = opts$scale
        ),
        SIMPLIFY = FALSE
    )
    
    have_passed <- vapply(
        matching_list,
        function(x) nrow(x) == 0,
        logical(1)
    )
    
    failed_values <- matching_list[!have_passed]
    
    body <- list()
    for( var in names(failed_values)){
        body <- append(
            body, 
            list(
                paste0("Variable: ", var),
                failed_values[[var]],
                ""
            )
        )
    }
    
    disp <- display$new(
        title = "Value Mismatches",
        body = body
    )
    
    browser()
    CR <- checkResult$new(
        name = "Values",
        display = disp, 
        result = ifelse(all(have_passed), "Passed", "Failed"), 
        message = "Not all Values Compared Equal", 
        data = failed_values
    )
    
    return(CR)
}



#' is_variable_different 
#' 
#' This subsets the data set on the variable name, picks out differences and returns a tibble
#' of differences for the given variable
#' @importFrom tibble as_tibble
#' @param variablename name of variable being compared
#' @param keynames name of keys
#' @param DAT Inputted dataset with base and compare vectors
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
#' @return A boolean vector which is T if target and current are different
is_variable_different <- function (variablename, keynames, DAT,  ...) {
    
    ### Dummy variable assignment to remove CRAN notes of no visible variable assignment
    
    cols <- paste0(variablename , c(".BASE", ".COMPARE"))
    vars <- c(keynames, cols[[1]] , cols[[2]])
    
    target <- DAT[[cols[[1]]]]
    current <- DAT[[cols[[2]]]]
    outvect <- find_difference(target, current, ...)
    
    x <- DAT[outvect, vars, with=FALSE]
    x2 <- setnames(x, c(cols[[1]], cols[[2]]) , c("Base", "Compare"))
    
    return(x2)
}



#' compare_vectors
#' 
#' Compare two vectors looking for differences
#' 
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
find_difference <- function (target, current, ...) {
    
    if( length(target) != length(current)){
        warning("Inputs are not of the same length")
        return(NULL)
    }
    
    if( is.null(target) | is.null(current) ){
        return( is.null(target) != is.null(current) )
    } 
    
    ### Initalise output, assume problem unless evidence otherwise
    return_vector <- rep(TRUE, length(target))
    
    nas_t <- is.na(target) 
    nas_c <- is.na(current)
    
    ## compare missing values
    nacompare <- nas_t != nas_c
    naselect <- nas_t|nas_c
    return_vector[naselect]  <- nacompare[naselect]
    
    ## compare non-missing values
    selectvector <- as.logical( (!nas_t) * (!nas_c) )
    
    comparevect <- compare_vectors( 
        target[selectvector] ,
        current[selectvector], 
        ...
    )
    
    return_vector[selectvector] <- comparevect
    
    return(return_vector)
}







#' compare_vectors.default
#' 
#' Default method, if the vector is not numeric or factor. Basic comparison
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
compare_vectors.default <- function(target, current, ...){
    target != current 
}




#' compare_vectors.factor
#' 
#' Compares factors. Sets them as character and then compares
#' @param target the base vector
#' @param current a vector to compare target to
#' @param ...  Additional arguments which might be passed through (numerical accuracy)
compare_vectors.factor <- function(target, current, ...){
    as.character(target) != as.character(current) 
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
    out <- target == current
    
    if (all(out)) {
        return(!out)
    }
    
    if (is.integer(target) || is.integer(current)){ 
        target <- as.double(target)
        current <- as.double(current)
    }
    
    xy <- abs(target - current)
    
    if (!is.null(scale)) {
        xy <- xy/scale
    }
    
    return(xy > tolerance)
    
}


