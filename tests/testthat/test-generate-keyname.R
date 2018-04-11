context("Test generate keyname")

nonetaken <-  data.frame(a = 1)
onetaken <- data.frame(..ROWNUMBER.. = 1)
twotaken <- data.frame(..ROWNUMBER.. = 1, ..RN.. = 1)
threetaken <- data.frame(..ROWNUMBER.. = 1, ..RN.. = 1,  ..ROWN.. = 1)
fourtaken <- data.frame(..ROWNUMBER.. = 1, ..RN.. = 1, ..ROWN.. = 1, ..N..= 1)


test_that("Correct name returned",{
    expect_equal("..ROWNUMBER..", generate_keyname(nonetaken, nonetaken))
    
    expect_equal("..RN..", generate_keyname(onetaken, onetaken))
    expect_equal("..RN..", generate_keyname(onetaken, nonetaken))
    expect_equal("..RN..", generate_keyname(nonetaken, onetaken))
    
    expect_equal("..ROWN..", generate_keyname(twotaken, onetaken))
    expect_equal("..ROWN..", generate_keyname(twotaken, nonetaken))
    expect_equal("..ROWN..", generate_keyname(twotaken, twotaken))
    expect_equal("..ROWN..", generate_keyname(onetaken, twotaken))
    expect_equal("..ROWN..", generate_keyname(onetaken, twotaken))
    
    expect_equal("..N..", generate_keyname(threetaken, onetaken))
    expect_equal("..N..", generate_keyname(threetaken, nonetaken))
    expect_equal("..N..", generate_keyname(threetaken, twotaken))
    expect_equal("..N..", generate_keyname(nonetaken, threetaken))
    expect_equal("..N..", generate_keyname(onetaken, threetaken))
    expect_equal("..N..", generate_keyname(twotaken, threetaken))
    expect_equal("..N..", generate_keyname(threetaken, threetaken))

}
)


test_that("Error if all names taken",{

    expect_error( generate_keyname(fourtaken, onetaken), "All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    expect_error( generate_keyname(fourtaken, nonetaken), "All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    expect_error( generate_keyname(fourtaken, twotaken), "All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    expect_error( generate_keyname(fourtaken, threetaken), "All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    expect_error( generate_keyname(onetaken, fourtaken), "All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    expect_error( generate_keyname(twotaken, fourtaken), "All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    expect_error( generate_keyname(nonetaken, fourtaken), "All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    expect_error( generate_keyname(fourtaken, fourtaken), "All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    
}
)