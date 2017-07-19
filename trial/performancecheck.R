identify_variable_diff_vcomp <- function( VAR, DAT , KEYS){
  cname <- paste0(VAR , c(".x" , ".y"))
  
  tryCatch({DAT %>%
      select_( .dots =  c(KEYS , cname)) %>%
      rename_( .dots = set_names( as.list(cname),  c("BASE" , "COMPARE"))) %>%
      mutate( VARIABLE = VAR ) %>%
      select_(.dots =  c("VARIABLE" , KEYS , "BASE" , "COMPARE" )) %>%
      filter ( vectorcompare(BASE , COMPARE))},
      warning = function(w){
        DAT %>%
          select_( .dots =  c(KEYS , cname)) %>%
          rename_( .dots = set_names( as.list(cname),  c("BASE" , "COMPARE"))) %>%
          mutate( VARIABLE = VAR ) %>%
          select_(.dots =  c("VARIABLE" , KEYS , "BASE" , "COMPARE" )) %>%
          filter ( !map2_lgl(BASE , COMPARE , check_is_equal))
      },
      error = function(e){
        DAT %>%
          select_( .dots =  c(KEYS , cname)) %>%
          rename_( .dots = set_names( as.list(cname),  c("BASE" , "COMPARE"))) %>%
          mutate( VARIABLE = VAR ) %>%
          select_(.dots =  c("VARIABLE" , KEYS , "BASE" , "COMPARE" )) %>%
          filter ( !map2_lgl(BASE , COMPARE , check_is_equal))
      })
  
}

identify_variable_diff_isequal<- function( VAR, DAT , KEYS){
  cname <- paste0(VAR , c(".x" , ".y"))

        DAT %>%
          select_( .dots =  c(KEYS , cname)) %>%
          rename_( .dots = set_names( as.list(cname),  c("BASE" , "COMPARE"))) %>%
          mutate( VARIABLE = VAR ) %>%
          select_(.dots =  c("VARIABLE" , KEYS , "BASE" , "COMPARE" )) %>%
          filter ( !map2_lgl(BASE , COMPARE , check_is_equal))

}


returndata <- function(x,y)
{
  DAT <- tibble(testvar.x = x, testvar.y =y , keys=seq(1,length(x)))
  DAT
}

N<-1e6
M<-1e3

bigvector <- rnorm(N)
#bigvector <- rep('cat',N)
bigvector2 <- bigvector
bigvector2[sample(seq(1,N),M,replace=FALSE)]<-rnorm(M)

DATcomp <-returndata(bigvector,bigvector)


start <-Sys.time()
identify_variable_diff_vcomp('testvar', DATcomp, 'keys')
end <-Sys.time()



start2 <-Sys.time()
identify_variable_diff_isequal('testvar', DATcomp, 'keys')
end2 <-Sys.time()

end-start

end2-start2






