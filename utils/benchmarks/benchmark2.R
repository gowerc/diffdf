


library(tibble)
library(diffdf)
library(purrr)
library(stringi)



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



### Generate test datasets
nrow <- 1000000
ncol <- 60

d1_num <- get_test_data(nrow, ncol, "num")
d1_chr <- get_test_data(nrow, ncol, "chr")
d1_fct <- get_test_data(nrow, ncol, "fct")
d1_all <- get_test_data(nrow, 20, c("num", "chr", "fct"))

d2_num <- get_test_data(nrow, ncol, "num")
d2_chr <- get_test_data(nrow, ncol, "chr")
d2_fct <- get_test_data(nrow, ncol, "fct")
d2_all <- get_test_data(nrow, 20, c("num", "chr", "fct"))


get_time <- function(d1, d2, n){
    rerun( n, system.time(diffdf(d1 , d1, suppress_warnings = T))[[3]]) %>% flatten_dbl() %>% mean
}

rerun_n <- 5

c1 <- list(
    num_same = get_time(d1_num , d1_num, rerun_n),
    num_diff = get_time(d1_num , d2_num, rerun_n),
    chr_same = get_time(d1_chr , d1_chr, rerun_n),
    chr_diff = get_time(d1_chr , d2_chr, rerun_n),
    fct_same = get_time(d1_fct , d1_fct, rerun_n),
    fct_diff = get_time(d1_fct , d2_fct, rerun_n),
    all_same = get_time(d1_all , d1_all, rerun_n),
    all_diff = get_time(d1_all , d2_all, rerun_n)
)


devtools::load_all()


c2 <- list(
    num_same = get_time(d1_num , d1_num, rerun_n),
    num_diff = get_time(d1_num , d2_num, rerun_n),
    chr_same = get_time(d1_chr , d1_chr, rerun_n),
    chr_diff = get_time(d1_chr , d2_chr, rerun_n),
    fct_same = get_time(d1_fct , d1_fct, rerun_n),
    fct_diff = get_time(d1_fct , d2_fct, rerun_n),
    all_same = get_time(d1_all , d1_all, rerun_n),
    all_diff = get_time(d1_all , d2_all, rerun_n)
)




get_print <- function(x){
    for( i in names(x)){
        cat( paste0( i , " - ", round(x[[i]],3), "\n"))
    }
}

get_print(c1)

get_print(c2)















