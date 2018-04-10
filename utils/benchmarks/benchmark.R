library(dplyr)
library(dfdiff)

benchmark <- function(nrow, ncol, seedin, same = T){
  set.seed(seedin)
  data1 <- rnorm(nrow*ncol)
  X<- matrix(data1, ncol = ncol) %>% as.data.frame()
  if (!same){
  data2 <- rnorm(nrow*ncol)
  Y<- matrix(data2, ncol = ncol) %>% as.data.frame()
  start_time <- Sys.time()
  compob <- suppressMessages(suppressWarnings(dfdiff(X,Y)))
  end_time <- Sys.time()
  }else{
    start_time <- Sys.time()
    compob <- suppressMessages(suppressWarnings(dfdiff(X,X)))
    end_time <- Sys.time()
  }
  end_time - start_time
}


seedset <- 10112017


benchmark(100, 500, seedset, T)
benchmark(100, 500, seedset, F)

benchmark(100000, 100, seedset, T)
benchmark(100000, 100, seedset, F)
benchmark(1000, 1000, seedset, F)


