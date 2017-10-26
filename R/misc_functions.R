



fix_factor_vars <- function( dsin , vars){
    for (var in vars){
        if(  is.factor(dsin[[var]])){
            dsin[[var]] <- as.character(dsin[[var]])
        }
    }
    return(dsin)
}


class_adder <- function(objectin, new_class, message, checkfun, order){
  class(objectin) <- append(new_class, class(objectin))
  attr(objectin, 'message') <- message
  attr(objectin, 'checkfun') <- checkfun
  attr(objectin, 'order') <- order
  objectin
}

nonempty_list <- function(in_list){
  noenmpty_list <- in_list[map(in_list, nrow)>0]
  noenmpty_list
}

checklength <- function(in_list){
  length(nonempty_list(in_list))
}
