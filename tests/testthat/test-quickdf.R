context("Test quick df correctly makes a data frame")
    
    test_that("Data frame working as expected",{
        
        one <- seq(1,10)
        a <- rnorm(10)
        b <- rnorm(10)
        c <- rnorm(10)
        d <- letters[1:10]
        two <- rnorm(10)
        three <- runif(10)
        four <- rep(T, 10)
        
        qdf <- quickdf(c(
            list(one = one),
            data.frame(
                a = a,
                b = b
            ),
            list(two =  two,
                 three = three),
            data.frame(c = c,
                       d = d),
            list(four = four)
        ))
        
        proper_df <- data.frame(
            one = one,
            a = a,
            b = b,
            two = two,
            three = three,
            c = c,
            d = d,
            four = four
        )
        
        expect_equal(qdf, proper_df)
        
    })
    