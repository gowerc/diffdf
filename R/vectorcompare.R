vectorcompare_wrapfun<-function (target, current, ...) {
  UseMethod("vectorcompare")
}

changemode <-function(targ, current)
{
  mcurr <- mode(current)
  eval(parse(text = paste0('as.',mcurr,'(targ)')))
}

mrank <-function(x)
{
  mx <- mode(x)
  case_when(is.factor(x) ~1,
            mx == 'character' ~4,
            mx == 'numeric' ~3,
            mx == 'logical' ~2)
}



vectorcompare<-function (target, current, ...) {
  if (is.null(target)|is.null(current))
  {
    target == current | is.null(target) == is.null(current)
  }else{
   
    #code to switch data to be same mode and switch factor to non factor
    
    ranktar <- mrank (target)
    rankcur <- mrank(current)
    if (ranktar >rankcur)
    {
      current <-changemode(current, target)
  } else if (ranktar <rankcur){
    target <-changemode(target, current)
  }  
  
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
  out <- out | target == current
  if (all(out)) {
    if (is.null(msg)) 
      return(rep(FALSE, length(out)))
    else return(msg)
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
  if (is.na(xy) || xy > tolerance) 
    msg <- !out
  if (is.null(msg)) 
    rep(FALSE,length(out))
  else msg
}

 
x<-c(1,2,4,NA)
y<-c(1,2,NA,NA)

vectorcompare(x,y)

