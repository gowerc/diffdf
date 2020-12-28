

context("Testing generate keyname")


nonetaken <-  data.frame(a = 1)
onetaken <- data.frame(..ROWNUMBER.. = 1)
twotaken <- data.frame(..ROWNUMBER.. = 1, ..RN.. = 1)
threetaken <- data.frame(..ROWNUMBER.. = 1, ..RN.. = 1,  ..ROWN.. = 1)
fourtaken <- data.frame(..ROWNUMBER.. = 1, ..RN.. = 1, ..ROWN.. = 1, ..N..= 1)
allbaronetaken <- data.frame( ..RN.. = 1, ..ROWN.. = 1, ..N..= 1)


test_that("Correct name returned",{
    
    DATASETS <- list(
        
        list("..ROWNUMBER..", nonetaken, nonetaken),
        list("..ROWNUMBER..", allbaronetaken, allbaronetaken),
        
        list("..RN..", onetaken,  onetaken),
        list("..RN..", onetaken,  nonetaken),
        list("..RN..", nonetaken, onetaken),
        
        list("..ROWN..", twotaken, onetaken),
        list("..ROWN..", twotaken, nonetaken),
        list("..ROWN..", twotaken, twotaken),
        list("..ROWN..", onetaken, twotaken),
        list("..ROWN..", onetaken, twotaken),
        
        list("..N..", threetaken, onetaken),
        list("..N..", threetaken, nonetaken),
        list("..N..", threetaken, twotaken),
        list("..N..", nonetaken, threetaken),
        list("..N..", onetaken, threetaken),
        list("..N..", twotaken, threetaken),
        list("..N..", threetaken, threetaken)
    )
    
    for ( i in DATASETS){
        expect_equal( i[[1]], generate_keyname(i[[2]] , i[[3]]))    
    }
})


test_that("Error if all names taken in at least 1 datasets",{

    DATASETS <- list(
        list(fourtaken, onetaken), 
        list(fourtaken, nonetaken),
        list(fourtaken, twotaken),
        list(fourtaken, threetaken),
        list(onetaken, fourtaken),
        list(twotaken, fourtaken),
        list(nonetaken, fourtaken),
        list(fourtaken, fourtaken)
    )
    
    for ( i in DATASETS){
        expect_error(
            generate_keyname( i[[1]] , i[[2]]),
            "All default row names are in use in BASE/COMPARE. Please provide a KEY argument"    
        )
    }
})













