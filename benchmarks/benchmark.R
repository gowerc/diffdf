benchmark <- function()




X<-matrix(rnorm(10e7),ncol=1000) %>% as.data.frame()
Y<-matrix(rnorm(10e7),ncol=1000) %>% as.data.frame()
rcompare(X,Y)