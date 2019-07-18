library(tibble)
library(purrr)
library(stringi)
devtools::load_all()

get_test_data <- function(nrow, ncol, types = "num"){
    dat <- tibble( id = 1:nrow)
    for ( i in 1:ncol){
        if( "num" %in% types){
            dat[[paste0("num_",i)]] <- rnorm(nrow)
        }
        if( "chr" %in% types){
            chrs <- stri_rand_strings(500,50)
            dat[[paste0("chr_",i)]] <- sample(chrs, size = nrow , replace = T)
        }
        if( "fct" %in% types){
            fct <-  stri_rand_strings(10, 20)
            dat[[paste0("fct_",i)]] <- factor(sample(fct, size = nrow , replace = T) , labels = fct)
        }
    }
    return(dat)
}

get_print <- function(x){
    for( i in names(x)){
        cat( paste0( i , " - ", round(x[[i]],3), "\n"))
    }
}

get_times <- function(nrow, ncol, nrep){
    get_time <- function(d1, d2, n) rerun( n, system.time(diffdf(d1 , d1, suppress_warnings = T))[[3]]) %>% flatten_dbl() %>% mean
    dat <- list(
        d1_num = get_test_data(nrow, ncol, "num"),
        d1_chr = get_test_data(nrow, ncol, "chr"),
        nrep = nrep
    )
    list(
        num_same = get_time(dat$d1_num , dat$d1_num, dat$nrep),
        chr_same = get_time(dat$d1_chr , dat$d1_chr, dat$nrep)
    )
}

t1 <- get_times(10000000, 30, 1)
t2 <- get_times(10000000, 50, 1)
t3 <- get_times(10000000, 70, 1)

get_print(t1)
get_print(t2)
get_print(t3)


library(data.table)


c1 <- get_test_data(400000, 25, "chr")
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







