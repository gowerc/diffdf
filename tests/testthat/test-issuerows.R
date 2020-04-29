



context("Testing diffdf_issuerows")



expect_all_true <- function(object, ...) expect_true( all(object) ,...)
expect_any_true <- function(object, ...) expect_true( any(object) ,...)

expect_all_false <- function(object, ...) expect_false( all(object) ,...)
expect_any_false <- function(object, ...) expect_false( any(object) ,...)




test_that( "Equal datasets return no rows", {
    diffdf_options(warnings = FALSE)
    x <- list_of_comparisons[["Identical"]]
    diff <- diffdf( x[[1]] , x[[2]])
    dat <- diffdf_issuerows(x[[1]] , diff)
    expect_equal( nrow(dat),  0 )
    expect_equal( ncol(dat),  ncol(x[[1]]) )
    diffdf_options_reset()
})

 


test_that( "Checking no rows returned if only issue is missing cols" ,{
    diffdf_options(warnings = FALSE)
    x1 <- list_of_comparisons[["Missing Columns"]][[1]]
    x2 <- list_of_comparisons[["Missing Columns"]][[2]]
    diff <- diffdf( x1 , x2 )
    dat1 <- diffdf_issuerows(x1 , diff)
    dat2 <- diffdf_issuerows(x2 , diff)
    expect_equal(  nrow(dat1) , 0)
    expect_equal(  nrow(dat2) , 0)
    expect_equal(  ncol(dat1) , ncol(x1))
    expect_equal(  ncol(dat2) , ncol(x2))
    expect_true( ncol(dat1) != ncol(dat2))
    diffdf_options_reset()
})


test_that( "Checking no rows returned if only issue is missing rows" , {
    diffdf_options(warnings = FALSE)
    x1 <- list_of_comparisons[["Missing Rows"]][[1]]
    x2 <- list_of_comparisons[["Missing Rows"]][[2]]
    diff <- diffdf( x1 , x2 )
    dat1 <- diffdf_issuerows(x1 , diff)
    dat2 <- diffdf_issuerows(x2 , diff)
    expect_equal(  nrow(dat1) , 0)
    expect_equal(  nrow(dat2) , 0)
    expect_equal(  ncol(dat1) , ncol(x1))
    expect_equal(  ncol(dat2) , ncol(x2))
    expect_equal(  ncol(dat1) , ncol(dat2))
    diffdf_options_reset()
})




test_that( "Checking no rows returned if only issue is different attribs" ,{
    diffdf_options(warnings = FALSE)
    x1 <- list_of_comparisons[["Different attributes"]][[1]]
    x2 <- list_of_comparisons[["Different attributes"]][[2]]
    diff <- diffdf( x1 , x2)
    dat1 <- diffdf_issuerows(x1 , diff)
    dat2 <- diffdf_issuerows(x2 , diff)
    expect_equal(  nrow(dat1) , 0)
    expect_equal(  nrow(dat2) , 0)
    expect_equal(  ncol(dat1) , ncol(x1))
    expect_equal(  ncol(dat2) , ncol(x2))
    expect_equal(  ncol(dat1) , ncol(dat2))
    diffdf_options_reset()
})


test_that( "Checking correct values return where value differences are found", {
    diffdf_options(warnings = FALSE)
    x1 <- list_of_comparisons[["Different Values"]][[1]]
    x2 <- list_of_comparisons[["Different Values"]][[2]]
    diff <- diffdf( x1 , x2)
    dat1 <- diffdf_issuerows(x1 , diff)
    dat2 <- diffdf_issuerows(x2 , diff)
    expect_equal(  nrow(dat1) , 3)
    expect_equal(  nrow(dat2) , 3)
    expect_equal(  ncol(dat1) , ncol(x1))
    expect_equal(  ncol(dat2) , ncol(x2))
    expect_equal(  ncol(dat1) , ncol(dat2))
    expect_all_true( dat1$CONTINUOUS != c(1,2,3) )
    expect_all_true( dat2$CONTINUOUS == c(1,2,3) ) 
    diffdf_options_reset()
})




test_that( "can handle multple errors including key vars",{
    x1 <- list_of_comparisons[["everything"]][[1]]
    x2 <- list_of_comparisons[["everything"]][[2]]
    diffdf_options(warnings = FALSE)
    
    diff <- diffdf( x1 , x2 , c("ID" , "GROUP1"))
    dat1 <- diffdf_issuerows(x1 , diff)
    dat2 <- diffdf_issuerows(x2 , diff)
    expect_equal(  nrow(dat1) , 20)
    expect_equal(  nrow(dat2) , 20)
    expect_equal(  ncol(dat1) , ncol(x1))
    expect_equal(  ncol(dat2) , ncol(x2))
    expect_equal(  ncol(dat1) , ncol(dat2))
    
    
    dat1 <- diffdf_issuerows(x1 , diff, var = "CONTINUOUS")
    dat2 <- diffdf_issuerows(x2 , diff, var = "CONTINUOUS")
    expect_equal(  nrow(dat1) , 3)
    expect_equal(  nrow(dat2) , 3)
    expect_equal(  ncol(dat1) , ncol(x1))
    expect_equal(  ncol(dat2) , ncol(x2))
    expect_equal(  ncol(dat1) , ncol(dat2))
    expect_all_true( dat1$CONTINUOUS != c(1,2,3) )
    expect_all_true( dat2$CONTINUOUS == c(1,2,3) )
    
    
    ### No duplicate rows
    dat1 <- diffdf_issuerows(x1 , diff, var = c("CONTINUOUS", "CATEGORICAL"))
    dat2 <- diffdf_issuerows(x2 , diff, var = c("CONTINUOUS", "CATEGORICAL"))
    expect_equal(  nrow(dat1) , 9)
    expect_equal(  nrow(dat2) , 9)
    expect_equal(  ncol(dat1) , ncol(x1))
    expect_equal(  ncol(dat2) , ncol(x2))
    expect_equal(  ncol(dat1) , ncol(dat2))
    

    expect_error(
        diffdf_issuerows( iris , diff),
        "df does not contain all variables specified as keys in diff"
    )
    diffdf_options_reset()
})




test_that( "Correct columns are returned where both values and columns have issues",{
    diffdf_options(warnings = FALSE)
    x1 <- list_of_comparisons[["Missing Columns"]][[1]]
    x2 <- list_of_comparisons[["Missing Columns"]][[2]]
    x2$INTEGER <- as.integer(x2$INTEGER) 
    diff <- diffdf( x1 , x2 , "ID")
    dat1 <- diffdf_issuerows(x1 , diff)
    dat2 <- diffdf_issuerows(x2 , diff)
    expect_equal(  nrow(dat1) , 3)
    expect_equal(  nrow(dat2) , 3)
    expect_equal(  ncol(dat1) , ncol(x1))
    expect_equal(  ncol(dat2) , ncol(x2))
    expect_true( ncol(dat1) != ncol(dat2))
    diffdf_options_reset()
    
})




