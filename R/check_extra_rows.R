

check_extra_rows <- function(dfa, dfb, keys, name, message, type){
    
    dat_a <- copy(dfa)[,keys, with = FALSE]
    dat_b <- copy(dfb)[,keys, with = FALSE]
    
    index <- generate_keys(dat_a, dat_b)
    
    extra <- setDT(dat_a)[!dat_b, on = keys]

    ER <- list()
    ER[[type]] <- extra[[index]]
    
    disp <- display$new(
        title = paste0("Extra Rows in ", totitle(type)),
        body = list(extra[, keys, with = FALSE])
    )
    
    CR <- checkResult$new(
        name = name,
        display = disp,
        result = ifelse(nrow(extra) == 0, "Passed", "Failed"), 
        message = message, 
        data = extra[, keys, with = FALSE],
        exclude_rows = ER
    )
    
    return(CR)
}

check_extra_rows_base <- function(base, comp, keys, opts){
    check_extra_rows(
        base,
        comp,
        keys,
        "ExtraRowsBase",
        "There are rows in Base that are not in Compare",
        "base"
    )
}

check_extra_rows_comp <- function(base, comp, keys, opts){
    check_extra_rows(
        comp,
        base,
        keys,
        "ExtraRowsComp",
        "There are rows in Compare that are not in Base",
        "comp"
    )
}




