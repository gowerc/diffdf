context("Testing cpp functions work as expected")

    
test_that( "doublediff correctly flags differences", {
    expect_equal( doublediff( VALS$num, VALS$num_na, tolerance = sqrt(.Machine$double.eps)) , c(F,F,F,F,T))
    expect_equal( doublediff( VALS$flt, VALS$flt2, tolerance = sqrt(.Machine$double.eps))   , c(F,F,F,F,F))
})

test_that("doublediff correctly uses tolerances/scale arguments",{
    expect_equal( doublediff( VALS$flt, VALS$flt2, tolerance = sqrt(.Machine$double.eps)) , c(F,F,F,F,F))
    expect_equal( doublediff( VALS$flt, VALS$flt2, tolerance = 0.0000000000001 ) , c(T,T,T,T,T)) 
    expect_equal( doublediff( VALS$flt, VALS$flt3, tolerance = sqrt(.Machine$double.eps)) , c(F,F,F,F,F)) 
    expect_equal( doublediff( VALS$flt, VALS$flt3, tolerance = 0.0000000000001) , c(T,F,F,F,F)) 
    expect_equal( doublediff( VALS$flt, VALS$flt2, tolerance = 1,
                                   scale = 1e-13) , c(T,T,T,T,T)) 
    expect_equal( doublediff( VALS$flt, VALS$flt_calc, tolerance = sqrt(.Machine$double.eps)) , c(F,F,F,F,F)) 
    expect_equal( doublediff( VALS$flt, VALS$flt_calc, tolerance = 1e-17) , VALS$flt!=VALS$flt_calc) 
})

test_that("String diff correctly flags differences",{
    expect_equal( stringdiff( VALS$chr, VALS$chr_na) , c(F,F,F,T,T))
    expect_equal( stringdiff( 
        c("bcd", "a_e_[]{},.<>?/~@#", paste(sample(letters, 100000, replace = T), collapse = "")), 
        c("bce", "a_e_[]{},.<>?/~@#", paste(sample(letters, 100000, replace = T), collapse = ""))
    ), c(T, F, T))
})

test_that("find_matches correctly identifies the index of matches",{
    ds1 <- data_frame(
        a = seq(1, 10),
        b = letters[1:10],
        c = rep(c(T, F),c(5,5))
    )
    ds2 <- ds1
    ds2[1,1] <- 4
    ds2[8,3] <- T
    ds2[6,2] <- "blue"
    ds2 <- ds2[do.call("order", ds2),]
    
    matches <- find_matches(ds1, ds2, c("numeric", "character", "logical"), 3)
    
    expect_equal(matches[[1]],
                 c(2,3,4,5,7,9,10))
    
    bigds <- data_frame(
        a = seq(1, 1000),
        b = rep(c("a", "b"), 500),
        c = rep(T, 1000)
    )
    small_ds<- data_frame(
        a = c(451, 452, 453),
        b = c("a", "b", "c"),
        c = c(F, T, T)
    )
                 
    expect_equal(find_matches( bigds, small_ds, c("numeric", "character", "logical"), 3),
                 list(
                     c(452),
                     c(2)
                 ))
    
    
})    