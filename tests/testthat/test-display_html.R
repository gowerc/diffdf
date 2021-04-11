# options("testthat.progress.max_fails" = 999)

runme <- function(x){
    x2 <- diffdf(x[[1]] , x[[2]] , onfailure = "nothing")
    expect_snapshot_output( cat(as.character(x2, type = "html"), sep = "\n") )
}


test_that("HTML - Identical", {
    runme(list_of_comparisons$Identical)
    runme(list_of_comparisons$`Identical 2` )
})

test_that("HTML - Different Values",{
    runme(list_of_comparisons$`Different Values` )
    runme(list_of_comparisons$`Different Values 2` )
})


test_that("HTML - Different Attributes",{
    runme(list_of_comparisons$`Different attributes` )
    runme(list_of_comparisons$`Different attributes 2` )
})


test_that("HTML - Different Levels",{
    runme(list_of_comparisons$`Different Levels` )
    runme(list_of_comparisons$`Different Levels 2` )
})


test_that("HTML - Difference Class",{
    runme(list_of_comparisons$`Different Class` )
    runme(list_of_comparisons$`Different Class 2` )
})


test_that("HTML - Different Modes",{
    runme(list_of_comparisons$`Different Modes` )
    runme(list_of_comparisons$`Different Modes 2` )
})


test_that("HTML - Missing Columns",{
    runme(list_of_comparisons$`Missing Columns` )
    runme(list_of_comparisons$`Missing Columns 2` )
})


test_that("HTML - Missing Rows",{
    runme(list_of_comparisons$`Missing Rows` )
    runme(list_of_comparisons$`Missing Rows 2` ) 
})



test_that("HTML - Everything",{
    runme(list_of_comparisons$everything )
    runme(list_of_comparisons$`everything 2` )
})


test_that("HTML - Misc",{
    runme(list_of_comparisons$`Missing Vs NA` )
})
