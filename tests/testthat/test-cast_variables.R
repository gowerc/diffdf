context("Testing casting variables")

TDAT_CAT_ASCHR <- TDAT
TDAT_CAT_ASCHR$CATEGORICAL <- as.character(TDAT_CAT_ASCHR$CATEGORICAL) 


TDAT_INT_ASNUM <- TDAT
TDAT_INT_ASNUM$INTEGER <- as.numeric(TDAT_INT_ASNUM$INTEGER)


cast_check <- function(dataone, datatwo){
    cast_base <- cast_variables(dataone, datatwo)
    cast_comp <- cast_variables(datatwo, dataone)
    expect_equal(sapply(datatwo, class), sapply(cast_base$BASE, class))
    expect_equal(sapply(datatwo, class), sapply(cast_comp$COMP, class))
    expect_equal(sapply(datatwo, class), sapply(cast_base$COMP, class))
    expect_equal(sapply(datatwo, class), sapply(cast_comp$BASE, class))
}


test_that("Factor cast to character, no other changes!",
          {
              suppressWarnings(cast_check(TDAT, TDAT_CAT_ASCHR))
              expect_warning(cast_variables(TDAT, TDAT_CAT_ASCHR),
                             "Casting CATEGORICAL in base to character",
                             all = TRUE)
              expect_warning(cast_variables(TDAT_CAT_ASCHR, TDAT),
                             "Casting CATEGORICAL in compare to character",
                             all = TRUE)
          })

test_that("Int cast to numeric, no other changes!",
          {
              suppressWarnings(cast_check(TDAT, TDAT_INT_ASNUM))
              expect_warning(cast_variables(TDAT, TDAT_INT_ASNUM),
                             "Casting INTEGER in base to numeric",
                             all = TRUE)
              expect_warning(cast_variables(TDAT_INT_ASNUM, TDAT),
                             "Casting INTEGER in compare to numeric",
                             all = TRUE)
          })