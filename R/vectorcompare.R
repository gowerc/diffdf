vectorcompare_wrapfun<-function (target, current, ...) {
  UseMethod("vectorcompare")
}


vectorcompare<-function (target, current, ...) {
  if (is.null(target)|is.null(current))
  {
    return(is.null(target) == is.null(current))
  }else{
   
    #code to switch data to be same mode and switch factor to non factor
    
  
  N <- length(target)
  outvect <-rep(TRUE,N)
  
  
  nas_t <- is.na(target) 
  nas_c <- is.na(current)
  nacompare <- is.na(target) != is.na(current)
 
  selectvector <-as.logical((!nas_t)*(!nas_c))
  
  target  <- target[selectvector]
  current <- current[selectvector]  
  comparevect <-vectorcompare_wrapfun(target,current)
  outvect[selectvector]   <- comparevect
  outvect[nas_t|nas_c] <- nacompare[nas_t|nas_c]
  as.logical(outvect)
  
  
  }  
}
  
vectorcompare.default <- function(target, current, ...){
  target != current 
  
}

vectorcompare.factor <- function(target, current, ...){
  as.character(target) != as.character(current) 
  
}

vectorcompare.numeric <- function(target, current, tolerance = sqrt(.Machine$double.eps), 
                                  scale = NULL, ..., check.attributes = TRUE) 
{
  if (!is.numeric(tolerance)) 
    stop("'tolerance' should be numeric")
  if (!is.numeric(scale) && !is.null(scale)) 
    stop("'scale' should be numeric or NULL")
  if (!is.logical(check.attributes)) 
    stop(gettextf("'%s' must be logical", "check.attributes"), 
         domain = NA)
  target <- as.vector(target)
  current <- as.vector(current)
  out <- target == current
  if (all(out)) {
      return(rep(FALSE, length(out)))
  }
  N <- length(target)
  target <- target[!out]
  current <- current[!out]
  
  if (is.integer(target) && is.integer(current)) 
    target <- as.double(target)
  xy <- mean(abs(target - current))
  what <- if (is.null(scale)) {
    xn <- mean(abs(target))
    if (is.finite(xn) && xn > tolerance) {
      xy <- xy/xn
      "relative"
    }
    else "absolute"
  }
  else {
    xy <- xy/scale
    if (scale == 1) 
      "absolute"
    else "scaled"
  }
  msg <- NULL
  if (is.na(xy) || xy > tolerance) 
    msg <- !out
  if (is.null(msg)) 
    rep(FALSE,length(out))
  else msg
}

 


