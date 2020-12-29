
# TODO
test_that( "find_difference correctly doesn't flag identical objects", {
    
    expect_self <- function(x , name)  {
        expect_true( !find_difference( x , x) %>%  all, label = name ) 
    }
    
    purrr::walk2( VALS , names(VALS) , expect_self)
    
    expect_true( !find_difference( VALS$flt , VALS$flt_calc) %>%  all )
})



test_that( "find_difference correctly flags differences", {
    expect_equal( find_difference( VALS$num, VALS$num_na) , c(F,F,F,F,T))
    expect_equal( find_difference( VALS$flt, VALS$flt2)   , c(F,F,F,F,F))
    expect_equal( find_difference( VALS$chr, VALS$chr_na) , c(F,F,F,T,T))
    expect_equal( find_difference( VALS$fct, VALS$fct_na) , c(F,F,T,F,F))
    expect_equal( find_difference( VALS$lgl, VALS$lgl_na) , c(F,F,T,F,F))
    
})

test_that( "find_difference correctly handles date variables", {
    
    expect_equal( find_difference( VALS$dt, VALS$dt) , c(F, F, F, F))
    expect_equal( find_difference( VALS$dt, VALS$dt_na) , c(F, F, T, F))
    expect_equal( find_difference( VALS$dt_na, VALS$dt_na) , c(F, F, F, F))
    
    expect_equal( find_difference( VALS$dtm, VALS$dtm) , c(F,F,F))
    expect_equal( find_difference( VALS$dtm, VALS$dtm_na) , c(F,T,F))
    expect_equal( find_difference( VALS$dtm_na, VALS$dtm_na) , c(F,F,F))
    expect_equal( find_difference( VALS$dtm, VALS$dtm2) , c(T,T,F))
    expect_equal( find_difference( VALS$dtm_na, VALS$dtm2) , c(T,T,F))
    expect_equal( find_difference( VALS$dtm2, VALS$dtm2) , c(F,F,F))
    
    
})

test_that("find_difference correctly uses tolerances/scale arguments",{
    expect_equal( find_difference( VALS$flt, VALS$flt2) , c(F,F,F,F,F))
    expect_equal( find_difference( VALS$flt, VALS$flt2, tolerance = 0.0000000000001 ) , c(T,T,T,T,T)) 
    expect_equal( find_difference( VALS$flt, VALS$flt3) , c(F,F,F,F,F)) 
    expect_equal( find_difference( VALS$flt, VALS$flt3, tolerance = 0.0000000000001 ) , c(T,F,F,F,F)) 
    expect_equal( find_difference( VALS$flt, VALS$flt2, tolerance = 1,
                                   scale = 1e-13) , c(T,T,T,T,T)) 
    expect_equal( find_difference( VALS$flt, VALS$flt_calc) , c(F,F,F,F,F)) 
    expect_equal( find_difference( VALS$flt, VALS$flt_calc, tolerance = 1e-17) , VALS$flt!=VALS$flt_calc) 
})




test_that( "find_difference throws a warning if vectors are of a different length", {
    msg <- "Inputs are not of the same length"
    expect_warning(find_difference( c(1,2,3) , c(1,2,NULL)), regexp =  msg)
    expect_warning(find_difference( c(1,2,3) , c(1,2)), regexp =  msg)
})




test_that( "compare_vectors works for lists", {
    
    as_class <- function( x , cls){
        x2 <- list(x)
        class(x2) <- cls
        return(x2)
    }
    
    obj_attr1 <- TDAT
    obj_attr2 <- TDAT
    obj_attr3 <- TDAT
    
    attr(obj_attr1, "x") <- "word"
    attr(obj_attr2, "y") <- "word1"
    attr(obj_attr3, "y") <- "word2"
    
    p1 <- ggplot2::ggplot(TDAT, ggplot2::aes(x=CONTINUOUS, y = INTEGER, col = CATEGORICAL)) +
        ggplot2::geom_point()
    
    p2 <- ggplot2::ggplot(TDAT, ggplot2::aes(x=CONTINUOUS, y = INTEGER, col = CATEGORICAL)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw()
    
    mod1 <- lm( data = TDAT, CONTINUOUS ~ CATEGORICAL)
    mod2 <- lm( data = TDAT, CONTINUOUS ~ CATEGORICAL + INTEGER)
    mod3 <- lm( data = TDAT, CONTINUOUS ~ CATEGORICAL)
    
    
    obj1 <- list(
        as_class(c(1,2,3), "a"),
        as_class(c("a", "b"), "a"),
        as_class(c("a", "b"), "a"),
        as_class(c("a", "b"), "a"),
        obj_attr1,
        obj_attr2,
        p1,
        p1,
        mod1,
        mod1,
        mod1
    )
    
    obj2 <- list(
        as_class(c(1,2,3), "a"),
        as_class(c("a", "b"), "a"),  ## Same Values, Same Class
        as_class(c("a", "b"), "b"),  ## Same Values, Different Class
        as_class(c("a", "c"), "a"),  ## Different Values, Same Class
        obj_attr1,   ## Same values, same attributes
        obj_attr3,   ## Same values, different attributes
        p1,  ## Same plot
        p2,  ## Different plot
        mod1, ## Same model
        mod2, ## Different model
        mod3  ## same model stored in a different variable
    )
    
    ### Baseline testing
    expect_equal(
        find_difference( c(1,2,3), c(1,2,4)),
        c( FALSE , FALSE, TRUE)
    )
    
    expect_equal(
        find_difference(obj1,obj2),
        #  1       2     3     4      5     6      7     8      9     10     11
        c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
    )
    
})

