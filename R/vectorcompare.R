vectorcompare<-function (target, current, ...) {
  UseMethod("vectorcompare")
}
  
vectorcompare.default <- function(target, current, ...){
  target != current |
    ( is.na(target) != is.na(current))  |
    (is.null(target) != is.null(current))
  
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
  msg <- if (check.attributes) 
    attr.all.equal(target, current, tolerance = tolerance, 
                   scale = scale, ...)
  if (data.class(target) != data.class(current)) {
    msg <- c(msg, paste0("target is ", data.class(target), 
                         ", current is ", data.class(current)))
    return(msg)
  }
  lt <- length(target)
  lc <- length(current)
  cplx <- is.complex(target)
  if (lt != lc) {
    if (!is.null(msg)) 
      msg <- msg[-grep("\\bLengths\\b", msg)]
    msg <- c(msg, paste0(if (cplx) "Complex" else "Numeric", 
                         ": lengths (", lt, ", ", lc, ") differ"))
    return(msg)
  }
  target <- as.vector(target)
  current <- as.vector(current)
  out <- is.na(target)
  if (any(out != is.na(current))) {
    msg <- c(msg, paste("'is.NA' value mismatch:", sum(is.na(current)), 
                        "in current", sum(out), "in target"))
    return(msg)
  }
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
  xy <- mean((if (cplx) 
    Mod
    else abs)(target - current))
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
  if (cplx) 
    what <- paste(what, "Mod")
  if (is.na(xy) || xy > tolerance) 
    msg <- !out
  if (is.null(msg)) 
    rep(FALSE,length(out))
  else msg
}

 

