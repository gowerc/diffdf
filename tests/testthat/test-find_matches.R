context("Testing cpp functions work as expected")

    
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
    
    matches1 <-  find_matches(ds1, ds2, get_column_mode(ds1), 3)
    matches2 <-  find_matches(ds1, ds1, get_column_mode(ds1), 3)
    
    expect_equal( matches1[[1]], c(2,3,4,5,7,9,10))
    expect_equal( matches1[[2]], c(1, 2, 4, 5, 7, 9, 10))
    
    expect_equal( matches2[[1]], 1:nrow(ds1))
    expect_equal( matches2[[2]], 1:nrow(ds1))
    
    bigds <- tibble(
        a = seq(1, 1000),
        b = rep(c("a", "b"), 500),
        c = rep(T, 1000)
    )
    small_ds <- tibble(
        a = c(451, 452, 453),
        b = c("a", "b", "c"),
        c = c(F, T, T)
    )
                 
    expect_equal(
        find_matches( bigds, small_ds, get_column_mode(bigds), 3),
        list(c(452),c(2))
    )
    
    
    w_date <- tibble(
        a = letters[1:10],
        b = as.Date(seq(1, 1001, length = 10), origin = "1970-01-01")
    )
    
    w_date2 <- w_date
    w_date2[5,1] <- "diff"
    w_date2[7,2] <- as.Date("1980-01-07")
    w_date2[9,1] <- "new"
    w_date2[9,2] <- as.Date("1980-01-03")
    w_date2 <- sort_df( w_date2, c("b", "a"))
    
    matches <-  find_matches(w_date, w_date2, get_column_mode(w_date), 2)
    expect_equal(matches[[1]], c(1,2,3,4,6,8,10))
    expect_equal(matches[[2]], c(1,2,3,4,6,7,8))
    
    
    
    
})    

















