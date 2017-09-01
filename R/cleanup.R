cleanup <- function(inlist)
{
  dimzero <-function(X){

    out <- FALSE
    try({
      if(dim(X)[1] ==0 | dim(X)[2] == 0)
        out <- TRUE
    }, silent = TRUE)
    out
  }
  inlist[map_lgl( inlist, dimzero)] <- NULL
  inlist[['VarDiffs']][map_lgl(inlist[['VarDiffs']], dimzero)] <- NULL
  inlist
}