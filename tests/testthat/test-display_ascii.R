# options("testthat.progress.max_fails" = 999)

runme <- function(x, dfsummary = TRUE){
    x2 <- diffdf(x[[1]] , x[[2]] , onfailure = "nothing")
    expect_snapshot_output( cat(as.character(x2, type = "ascii", dfsummary = dfsummary), sep = "\n") )
}


test_that("Ascii - Identical", {
    runme(list_of_comparisons$Identical)
    runme(list_of_comparisons$`Identical 2` )
})

test_that("Ascii - Different Values",{
    runme(list_of_comparisons$`Different Values` )
    runme(list_of_comparisons$`Different Values 2` )
})


test_that("Ascii - Different Attributes",{
    runme(list_of_comparisons$`Different attributes` )
    runme(list_of_comparisons$`Different attributes 2` )
})


test_that("Ascii - Different Levels",{
    runme(list_of_comparisons$`Different Levels` )
    runme(list_of_comparisons$`Different Levels 2` )
})


test_that("Ascii - Difference Class",{
    runme(list_of_comparisons$`Different Class` )
    runme(list_of_comparisons$`Different Class 2` )
})


test_that("Ascii - Different Modes",{
    runme(list_of_comparisons$`Different Modes` )
    runme(list_of_comparisons$`Different Modes 2` )
})


test_that("Ascii - Missing Columns",{
    runme(list_of_comparisons$`Missing Columns` )
    runme(list_of_comparisons$`Missing Columns 2` )
})


test_that("Ascii - Missing Rows",{
    runme(list_of_comparisons$`Missing Rows` )
    runme(list_of_comparisons$`Missing Rows 2` ) 
})



test_that("Ascii - Everything",{
    runme(list_of_comparisons$everything )
    runme(list_of_comparisons$`everything 2` )
})


test_that("Ascii - Misc",{
    runme(list_of_comparisons$`Missing Vs NA` )
})


 
## No summary

test_that("Ascii - Different Values - No Summary",{
    runme(list_of_comparisons$`Different Values`, dfsummary = FALSE)
    runme(list_of_comparisons$`Different Values 2`, dfsummary = FALSE )
})
    
test_that("Ascii - Identical - No Summary", {
    runme(list_of_comparisons$Identical, dfsummary = FALSE)
    runme(list_of_comparisons$`Identical 2`, dfsummary = FALSE )
})
    
