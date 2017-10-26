



fix_factor_vars <- function( dsin , vars){
    for (var in vars){
        if(  is.factor(dsin[[var]])){
            dsin[[var]] <- as.character(dsin[[var]])
        }
    }
    return(dsin)
}



