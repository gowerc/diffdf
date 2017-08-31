

modediffs <-function(BASE, COMP, matching_cols){
  
  BASEmode <- BASE %>% select_(.dots = matching_cols) %>% map_chr(mode)
  COMPmode <- COMP %>% select_(.dots = matching_cols) %>% map_chr(mode)
  
  BASEclass <- BASE %>% select_(.dots = matching_cols) %>% map_chr(class)
  COMPclass <- COMP %>% select_(.dots = matching_cols) %>% map_chr(class)
  
  tibble(VARIABLE = matching_cols, BASEmode, COMPmode, BASEclass, COMPclass) %>% 
    filter(BASEmode != COMPmode | (BASEclass != COMPclass & (BASEclass == 'factor' | COMPclass == 'factor')))
  
  
}


attdiffs <- function(BASE, COMP, matching_cols, attin){
    
      att_BASE <-  BASE %>% select_(.dots = matching_cols) %>% map(attr, which = attin) %>%
         tibble(VARIABLE = matching_cols) %>%
          rename("BASEatt"='.') %>% mutate(isnull = map_lgl(BASEatt,is.null)) %>% filter(!isnull) %>% 
          select(VARIABLE, BASEatt)
      
        att_COMP <-  COMP %>% select_(.dots = matching_cols) %>% map(attr, which = attin) %>%
            tibble(VARIABLE = matching_cols) %>%
            rename_("COMPatt"='.') %>% mutate(isnull = map_lgl(COMPatt,is.null)) %>% filter(!isnull) %>% 
            select(VARIABLE, COMPatt)

        full_join(att_BASE, att_COMP , by = 'VARIABLE') %>%
          mutate(comparison = map2_lgl(BASEatt,COMPatt, identical)) %>% 
          filter(!comparison) %>%
          select(VARIABLE, BASEatt, COMPatt)
}

att_diffs <- function(BASE, COMP, matching_cols){
  
  BASE_att <- lapply(BASE, attributes) %>% 
    map_df(tibble::enframe, .id = 'VARIABLE') %>% 
    rename(BASEatt = value, attr_name = name) 
  COMP_att <- lapply(COMP, attributes)  %>% 
    map_df(tibble::enframe, .id = 'VARIABLE') %>% 
    rename(COMPatt = value, attr_name = name) 
  
  
  full_join(BASE_att, COMP_att , by = c('VARIABLE', 'attr_name')) %>% 
    mutate(comparison = map2_lgl(BASEatt,COMPatt, identical)) %>% filter(!comparison) %>%
    select(VARIABLE, attr_name, BASEatt, COMPatt)
}





vectorcompare_wrapfun<-function (target, current, ...) {
  UseMethod("vectorcompare")
}


vectorcompare<-function (target, current, ...) {
  if (is.null(target)|is.null(current))
  {
    return(is.null(target) == is.null(current))
  }else{
   

    
  
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
                                  scale = NULL, ..., check.attributes = TRUE) {
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
    }
  }
  else {
    xy <- xy/scale
  }
  msg <- NULL
  if (is.na(xy) || xy > tolerance) 
    msg <- !out
  if (is.null(msg)) 
    rep(FALSE,length(out))
  else msg
}

 


