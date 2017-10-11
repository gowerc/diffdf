# library(purrr)
# library(dplyr)
# library(testthat)
# source("./tests/testthat/helper-create_test_data.R")


##############################
## set up testing datasets


set.seed(101010223)

## Arbitary change some values
TDAT_INTCHANGE <- TDAT %>%
    mutate( INTEGER = ifelse(runif(nrow(.)) > 0.8 , INTEGER + 1 , INTEGER))

## Change other values to test
TDAT_CHARCHANGE <- TDAT %>%
    mutate(CHARACTER = ifelse(row_number() == 1, 'different', CHARACTER))

TDAT_DATECHANGE <- TDAT %>%
    mutate(DATE = as.Date(
        ifelse(
            row_number() == 1,
            as.Date("01/01/1981", format = "%d/%m/%Y"),
            DATE
        ),
        origin = "1970-01-01"
    ))

TDAT_LOGCHANGE <- TDAT %>%
    mutate(LOGICAL = ifelse(LOGICAL, F, T))

TDAT_FACTVALCHANGE <- TDAT %>%
    mutate(CATEGORICAL = factor(
        ifelse(CATEGORICAL == 'A', 'B', as.character(CATEGORICAL)),
        levels = c('A', 'B','C','D')
    ))

## add nas

TDAT_CHARCHANGENA <- TDAT %>%
    mutate(CHARACTER = ifelse(row_number() == 1, NA, CHARACTER))

TDAT_DATECHANGENA <- TDAT %>%
    mutate(DATE = (ifelse(row_number() == 1, NA, DATE)))

TDAT_LOGCHANGENA <- TDAT %>%
    mutate(LOGICAL = ifelse(LOGICAL, NA, T))

TDAT_FACTVALCHANGENA <- TDAT %>%
    mutate(CATEGORICAL = factor(
            ifelse(CATEGORICAL == 'A', NA, as.character(CATEGORICAL)),
            levels = c('A', 'B','C','D')
        )
    )

## add list column

TDAT_PLUSLIST <- TDAT %>%
    mutate(LIST = rep(list(CATEGORICAL) , nrow(.)))

##add change in mode

TDAT_MODECHANGE <- TDAT %>%
    mutate( INTEGER = as.character(INTEGER))


TDAT_FACTCHANGE <- TDAT %>%
    mutate( CATEGORICAL = factor(CATEGORICAL, levels = c(levels(CATEGORICAL), 'New')))

#switch integer to double, this shouldn't affect the comparison
TDAT_MODEDBL <- TDAT %>%
    mutate( INTEGER = as.double(INTEGER))

##add extra columns

TDAT_EXTCOLS <- TDAT %>%
    mutate(
        ext = CATEGORICAL,
        ext2 = CATEGORICAL
    )

##add extra rows

TDAT_EXTROWS <- bind_rows(TDAT, TDAT)


###################################
##Tests


test_that( "Check comparision of equal objects",{
    expect_false( rcompare(iris, iris)$Issues )
    expect_false( rcompare(TDAT, TDAT)$Issues )
    expect_false( rcompare(TDAT, TDAT, "ID")$Issues )
    expect_false( rcompare(TDAT, TDAT, c("GROUP1" , "GROUP2"))$Issues )
    expect_false( rcompare(TDAT, TDAT_MODEDBL)$Issues )
    expect_false( rcompare(TDAT_CHARCHANGENA , TDAT_CHARCHANGENA )$Issues )
    expect_false( rcompare(TDAT_DATECHANGENA , TDAT_DATECHANGENA )$Issues )
    expect_false( rcompare(TDAT_LOGCHANGENA , TDAT_LOGCHANGENA )$Issues )
    expect_false( rcompare(TDAT_FACTVALCHANGENA, TDAT_FACTVALCHANGENA)$Issues )
})

test_that( "Unequal objects raise warnings" , {

    msg <- "Not all values compared equal"

    expect_warning(
        rcompare(TDAT , TDAT_INTCHANGE),
        msg
    )

    expect_warning(
        rcompare(TDAT , TDAT_CHARCHANGE),
        msg
    )

    expect_warning(
        rcompare(TDAT , TDAT_DATECHANGE),
        msg
    )

    expect_warning(
        rcompare(TDAT , TDAT_LOGCHANGE),
        msg
    )

    expect_warning(
        rcompare(TDAT , TDAT_FACTVALCHANGE),
        msg
    )

    expect_warning(
        rcompare(TDAT , TDAT_CHARCHANGENA),
        msg
    )

    expect_warning(
        rcompare(TDAT , TDAT_DATECHANGENA),
        msg
    )

    expect_warning(
        rcompare(TDAT , TDAT_LOGCHANGENA),
        msg
    )

    expect_warning(
        rcompare(TDAT , TDAT_FACTVALCHANGENA),
        msg
    )
})

#checks for numeric differences

numdiffcheck <-function(compdat, target, value){

    rcompare_ob <-  rcompare(TDAT , compdat , suppress_warnings = T )$NumDiff
    rcompare_targ <- rcompare_ob[target] %>% as.numeric()
    rcompare_all <- rcompare_ob %>% sum() %>% as.numeric()

    expect_false(
        rcompare_targ == 0,
        info = 'targeted value wrong!'
    )

    expect_false(
        rcompare_all == 0 ,
        info = 'overall value wrong!'
    )
}

test_that( "Unequal object, checking numbers correct" , {
    numdiffcheck( TDAT_CHARCHANGE,      'CHARACTER')
    numdiffcheck( TDAT_DATECHANGE,      'DATE')
    numdiffcheck( TDAT_LOGCHANGE,       'LOGICAL')
    numdiffcheck( TDAT_FACTVALCHANGE,   'CATEGORICAL')
    numdiffcheck( TDAT_CHARCHANGENA,    'CHARACTER')
    numdiffcheck( TDAT_DATECHANGENA,    'DATE')
    numdiffcheck( TDAT_LOGCHANGENA,     'LOGICAL')
    numdiffcheck( TDAT_FACTVALCHANGENA, 'CATEGORICAL')
})

test_that( "Differing modes error" , {
    expect_warning(
        rcompare(TDAT , TDAT_MODECHANGE ),
        'There are Columns in BASE and COMPARE with different modes'
    )
})



test_that( "Differing factor levels error" , {
    expect_warning(
        rcompare(TDAT , TDAT_FACTCHANGE ),
        'There are Factor Columns in BASE and COMPARE with different levels'
    )
})




test_that("Non-Unique rows error", {
    expect_error(
        rcompare(TDAT , TDAT , "GROUP1"),
        'BY variables in BASE do not result in unique observations'
    )
})



test_that("Illegal columns error", {
    expect_warning(
        rcompare(TDAT_PLUSLIST, TDAT_PLUSLIST),
        'There are Columns in BASE with unsupported modes'
    )

    expect_warning(
        rcompare(TDAT_PLUSLIST, TDAT_PLUSLIST),
        'There are Columns in COMPARE with unsupported modes'
    )

    expect_warning(
        rcompare(TDAT, TDAT_PLUSLIST),
        'There are Columns in COMPARE with unsupported modes'
    )
})



test_that("Additional columns error", {

    expect_warning(
        rcompare(TDAT, TDAT_EXTCOLS),
        'There are Columns in COMPARE that are not in BASE'
    )

    expect_warning(
        rcompare(TDAT_EXTCOLS, TDAT),
        'There are Columns in BASE that are not in COMPARE'
    )

})



test_that("Additional rows error", {

    expect_warning(
        rcompare(TDAT, TDAT_EXTROWS),
        'There are Rows in COMPARE that are not in BASE'
    )

    expect_warning(
        rcompare(TDAT_EXTROWS, TDAT),
        'There are Rows in BASE that are not in COMPARE'
    )

})
