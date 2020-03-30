


context("Testing List Columns")


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
        compare_vectors( c(1,2,3), c(1,2,4)),
        c( FALSE , FALSE, TRUE)
    )
    
    expect_equal(
        compare_vectors(obj1,obj2),
        #  1       2     3     4      5     6      7     8      9     10     11
        c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
    )
    
})








test_that("List columns are supported", {
    
    #### Create list column on data
    TDAT_PLUSLIST <- TDAT 
    TDAT_PLUSLIST$LIST <- rep(list(TDAT$CATEGORICAL[1:5]) , nrow(TDAT))
    TDAT_PLUSLIST2 <- TDAT
    TDAT_PLUSLIST2$LIST <- rep(list(TDAT$CONTINUOUS[1:5]) , nrow(TDAT))
    
    expect_length( 
        diffdf(TDAT_PLUSLIST, TDAT_PLUSLIST),
        0
    )
    
    
    expect_warning(
        diffdf(TDAT_PLUSLIST, TDAT),
        'There are columns in BASE that are not in COMPARE'
    )
    
    expect_warning(
        diffdf(TDAT, TDAT_PLUSLIST),
        'There are columns in COMPARE that are not in BASE'
    )
    
    
    expect_warning(
        diffdf(TDAT_PLUSLIST, TDAT_PLUSLIST2),
        "Not all Values Compared Equal"
    )
    
    x <- diffdf(TDAT_PLUSLIST, TDAT_PLUSLIST2, warnings = FALSE)
    expect_equal(nrow(x$VarDiff_LIST),20)
    
})
















