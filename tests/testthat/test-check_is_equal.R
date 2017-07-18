library(dplyr)


test_that( "Can compare reals", {
    check_is_equal( 2 , 2)    %>% expect_true()
    check_is_equal( 2 , 1)    %>% expect_false()
    check_is_equal( 2 , NA)   %>% expect_false()
    check_is_equal( NA , 2)   %>% expect_false()
    check_is_equal( 2 , NULL) %>% expect_false()
    check_is_equal( NULL , 2) %>% expect_false()
})


test_that( "Can compare Advanced reals", {
    check_is_equal( 2.00000000001 , 2.00000000004 ) %>% expect_true()
    check_is_equal( 2.000000001   , 2.000000004 )   %>% expect_true()
    check_is_equal( 2.00000001    , 2.00000004 )    %>% expect_false()
    check_is_equal( 2.12345670    , 2.12345678)     %>% expect_false()
})

test_that( "Can compare characters", {
    check_is_equal( "a" , "a")  %>% expect_true()
    check_is_equal( "a" , "b")  %>% expect_false()
    check_is_equal( "a" , NA)   %>% expect_false()
    check_is_equal( NA , "a")   %>% expect_false()
    check_is_equal( "a" , NULL) %>% expect_false()
    check_is_equal( NULL , "a") %>% expect_false()
})

test_that( "Can compare logicals", {
    check_is_equal( T , T)    %>% expect_true()
    check_is_equal( F , F)    %>% expect_true()
    check_is_equal( T , F)    %>% expect_false()
    check_is_equal( F , T)    %>% expect_false()
    check_is_equal( T , NA)   %>% expect_false()
    check_is_equal( F , NA)   %>% expect_false()
    check_is_equal( T , NULL) %>% expect_false()
    check_is_equal( F , NULL) %>% expect_false()
})

test_that( "Can compare missings", {
    check_is_equal( NA , NA)     %>% expect_true()
    check_is_equal( NULL , NULL) %>% expect_true()
    check_is_equal( NA , NULL)   %>% expect_false()
    check_is_equal( NULL , NA)   %>% expect_false()
})




