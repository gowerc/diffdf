library(tibble)
library(purrr)
library(stringi)
library(data.table)
devtools::load_all()

get_test_data <- function(nrow, ncol, types){
    dat <- tibble( id = 1:nrow)
    i <- 1
    while (i <= ncol) {
        if( "num" %in% types){
            dat[[paste0("num_",i)]] <- rnorm(nrow)
            i <- i + 1
        }
        if( "chr" %in% types){
            chrs <- stri_rand_strings(500,50)
            dat[[paste0("chr_",i)]] <- sample(chrs, size = nrow , replace = T)
            i <- i + 1
        }
        if( "fct" %in% types){
            fct <-  stri_rand_strings(10, 20)
            dat[[paste0("fct_",i)]] <- factor(sample(fct, size = nrow , replace = T) , labels = fct)
            i <- i + 1
        }
    }
    return(dat)
}

get_print <- function(x){
    for( i in names(x)){
        cat( paste0( i , " - ", round(x[[i]],3), "\n"))
    }
}

timeit <- function(expr){
    start_time <- Sys.time()
    suppressWarnings(expr)
    stop_time <- Sys.time()
    return(difftime(stop_time, start_time, units = "secs"))
}

mean_rerun <- function(d1, d2, nrep){
    rerun( 
        nrep, 
        timeit(suppressWarnings({diffdf(d1 , d2)}))
    ) %>% 
        flatten_dbl() %>% 
        mean
}

get_times <- function(nrow, ncol, nrep){
    d1 = get_test_data(nrow, ncol, c("num", "chr", "fct"))
    d2 = get_test_data(nrow, ncol, c("num", "chr", "fct"))
    
    list(
        same = mean_rerun(d1, d2, nrep),
        diff = mean_rerun(d1 , d2, nrep)
    )
}

#### Compare run times
t1 <- get_times( 100000,  30, 4)
get_print(t1)

t2 <- get_times( 250000, 250, 4)
get_print(t2)

t3 <- get_times(5000000,  20, 4)
get_print(t3)




timeit(diffdf( c1, c1))


#### Manual inspection
c1 <- get_test_data(1000000, 50, c("chr", "num"))
profvis::profvis(diffdf( c1, c1), interval = 0.005)


t1 <- get_test_data(500, 10, "chr")
t2 <- get_test_data(500, 10, "chr")

diffdf( t1, t1, keys = "id")
diffdf( t1, t2, keys = "id")




chrs <- stri_rand_strings(500,50)
ch <- sample(chrs, size = 10000000 , replace = T)
system.time({
    x <- stringdiff(  ch, ch)
})



nums <- rnorm(10000000)
system.time({
    x <- doublediff(  nums,nums, tolerance = 1)
})







