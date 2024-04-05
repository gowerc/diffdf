

context("Testing print functionality")


# devtools::load_all()
# library(dplyr)
# library(stringr)
# library(devtools)
# library(testthat)
# source( "./tests/testthat/helper-create_test_data.R")


runme <- function(x){
    x2 <- diffdf(x[[1]] , x[[2]] , suppress_warnings = T)
    print(x2, as_string = TRUE)
}

RES <- map( list_of_comparisons , runme)


### Add additional examples that make use of keys

x <- diffdf(
    list_of_comparisons[["everything"]][[1]] ,
    list_of_comparisons[["everything"]][[2]] ,
    keys = "ID",
    suppress_warnings = T
)
RES[[ "With 1 key"]] <- print(x , as_string = TRUE)


x <- diffdf(
    list_of_comparisons[["everything"]][[1]] ,
    list_of_comparisons[["everything"]][[2]] ,
    keys = c("ID" , "GROUP1"),
    suppress_warnings = T
)
RES[["With 2 keys"]] <- print(x , as_string = TRUE)




SET_GOLD <- FALSE

if ( SET_GOLD ){
    TESTING_print_msg <- RES
    devtools::use_data( TESTING_print_msg , internal = TRUE , overwrite = TRUE)
} else {
    for ( i in seq_along(RES) ){
        expect_equal( 
            RES[[i]] , 
            TESTING_print_msg[[i]] , 
            info = paste0( "Reference = " , i , " - " , names(RES)[i])
        )
    }
}

test_that("row_limit works as expected", {
    diff <- diffdf(
      data.frame(col1 = LETTERS, col2 =  1:26),
      data.frame(col1 = LETTERS, col2 = 21:46),
      keys = 'col1',
      suppress_warnings = TRUE
    )
    output <- print(diff, as_string = TRUE)
    expect_length(output, 10+21)
    output <- print(diff, as_string = TRUE, row_limit = 26)
    expect_length(output, 26+21)
    output <- print(diff, as_string = TRUE, row_limit = 5)
    expect_length(output,  5+21)
    output <- print(diff, as_string = TRUE, row_limit = NULL)
    expect_length(output, 26+21)
})

test_that("print.diffdf errors when given bad inputs", {
    diff <- diffdf(
        data.frame(col1 = LETTERS, col2 =  1:26),
        data.frame(col1 = LETTERS, col2 = 21:46),
        keys = 'col1',
        suppress_warnings = TRUE
    )
    expect_error(
        print(diff, row_limit = 0),
              regexp = "row_limit should be a positive integer"
    )
    expect_error(
        print(diff, row_limit = "String"),
              regexp = "row_limit should be a numeric value or NULL"
    )
    expect_error(
        print(diff, row_limit = NA),
              regexp = "row_limit should be a numeric value or NULL"
    )
    expect_error(
        print(diff, row_limit = c(1,2)),
              regexp = "row_limit should have a length of 1"
    )
    
    expect_error(
        print(diff, as_string = "String"),
              regexp = "as_string should be a logical of length one"
    )
    expect_error(
        print(diff, as_string = c(TRUE, TRUE)),
              regexp = "as_string should be a logical of length one"
    )
})


# i <- 3
# print_tests[[i]]
# RES[[i]] %>% cat(sep = "\n")
# TESTING_print_msg[[i]] %>% cat(sep = "\n")
# 
# diffdf(
#  print_tests[[i]][[1]],
#  print_tests[[i]][[2]]
# )
# 
# for ( i in 1:length(RES)){
#     sink( paste0("./utils/print_output/output_",i,".txt"))
#     RES[[i]] %>% cat(sep = "\n")
#     sink()
# 
#     # sink( paste0("./utils/print_output/output_",i,"_gold.txt"))
#     # TESTING_print_msg[[i]] %>% cat(sep = "\n")
#     # sink()
# }

