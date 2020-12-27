
check_attributes <- function(base, comp, keys, opts){
    
    PROPS <- merge(
        x = get_properties(base),
        y = get_properties(comp), 
        by = "VARIABLE",  
        all = TRUE,
        sort = TRUE,
        suffixes = c(".BASE", ".COMP")
    )
    
    ### Setup dummy return value
    RETURN <- tibble(
        VARIABLE = character(),
        ATTR_NAME = character(),
        VALUES.BASE = list(),
        VALUES.COMP = list()
    )
    
    for ( i in  PROPS[["VARIABLE"]] ){
        
        PROPS_filt <- PROPS[get("VARIABLE") == i]
        
        ### Get a vector of all available attributes across both variables
        ATTRIB_NAMES = unique(c( 
            names(PROPS_filt[["ATTRIBS.BASE"]][[1]]), 
            names(PROPS_filt[["ATTRIBS.COMP"]][[1]])
        ))
        
        ### If variable has no attributes move onto the next variable
        if ( is.null(ATTRIB_NAMES) ) next()
        
        ### Loop over each attribute checking if they are identical and output any that aren't
        for ( j in ATTRIB_NAMES){
            
            ATTRIB_BASE = PROPS_filt[["ATTRIBS.BASE"]][[1]][j]
            ATTRIB_COMP = PROPS_filt[["ATTRIBS.COMP"]][[1]][j]
            
            if (!identical(ATTRIB_BASE, ATTRIB_COMP)){
                
                ATT_DIFFS <- tibble(
                    VARIABLE = i, 
                    ATTR_NAME = j, 
                    VALUES.BASE = ifelse( is.null(ATTRIB_BASE) , list() , ATTRIB_BASE),  
                    VALUES.COMP = ifelse( is.null(ATTRIB_COMP) , list() , ATTRIB_COMP)
                ) 
                
                RETURN <- rbind(RETURN, ATT_DIFFS)
            }
        }
    }
    
    CR <- checkResult$new(
        name = "Attributes",
        result = ifelse(nrow(RETURN) == 0, "Passed", "Failed"), 
        message = "There are columns in BASE and COMPARE with different attributes", 
        data = RETURN, 
        exclude_cols = RETURN[["VARIABLE"]]
    )
    
    return(CR)
    
}