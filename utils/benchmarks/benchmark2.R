devtools::load_all()

n <- 1000000
test_dat <- data_frame( id = 1:n)


vars <-letters[1:15]

for ( i in vars){
    test_dat[[paste0(i,"_num.x")]] <- rnorm(n)
    test_dat[[paste0(i,"_num.y")]] <- rnorm(n)
    test_dat[[paste0(i,"_chr.x")]] <- letters[ round(runif(n , 1,25))]
    test_dat[[paste0(i,"_chr.y")]] <- letters[ round(runif(n , 1,25))]
    test_dat[[paste0(i,"_fct.x")]] <- factor(test_dat[[paste0(i,"_chr.x")]])
    test_dat[[paste0(i,"_fct.y")]] <- factor(test_dat[[paste0(i,"_chr.y")]])
}

vars2 <- c(
    paste0(vars, "_num"),
    paste0(vars, "_chr"),
    paste0(vars, "_fct")
)

matching_cols = vars2
KEYS = "id"
DAT = test_dat
tolerance = sqrt(.Machine$double.eps)
scale = NULL

RES5 <- purrr::rerun( 10 , {
    x <- system.time({
        matching_list <- mapply(
            is_variable_different , 
            matching_cols,
            MoreArgs = list(
                keynames = KEYS, 
                datain = DAT, 
                tolerance = tolerance ,
                scale = scale
            ),
            SIMPLIFY = FALSE
        )
    })
    
    
    y <- system.time({
        HOLD <- list() 
        for ( v in vars2){
            
            xvar <- paste0(v,'.x')
            yvar <- paste0(v,'.y')
            
            keep <- find_difference( test_dat[[xvar]] , test_dat[[yvar]] , tolerance = tolerance , scale = scale)
            
            HOLD[[v]] <- data_frame(
                VARIABLE = v,
                BASE = test_dat[[xvar]][keep],
                COMPARE = test_dat[[yvar]][keep]
            )
            
            for ( i in KEYS){
                HOLD[[v]][[i]] <- test_dat[[i]][keep]
            }
            
            #HOLD[[v]] <- HOLD[[v]][ , c("VARIABLE" , KEYS, "BASE" , "COMPARE")]
        }
    })
    
    data_frame(
        x = x[[3]] ,
        y = y[[3]] ,
        diff = x - y,
        pcent = y / x
    )
    
})

bind_rows(RES1)$pcent %>% mean  # nrow = 300000 , ncol = 60
bind_rows(RES2)$pcent %>% mean  # nrow = 300000 , ncol = 15
bind_rows(RES3)$pcent %>% mean  # nrow = 600000 , ncol = 15
bind_rows(RES4)$pcent %>% mean
bind_rows(RES5)$pcent %>% mean


HOLD[["e_fct"]]
matching_list[["e_fct"]]




is_variable_different2 <- function (variablename, keynames, datain, ...) {
    

    
    if ( ! xvar %in% names(datain) | ! yvar %in% names(datain)){
        stop("Variable does not exist within input dataset")
    }
    
    target  <- datain[[xvar]]
    current <- datain[[yvar]]
    outvect <- find_difference(target, current, ...)
    
    datain[["VARIABLE"]] <- variablename
    
    names(datain)[names(datain) %in% c(xvar, yvar)] <- c("BASE", "COMPARE")
    
    as.tibble(subset(datain, outvect, select = c("VARIABLE", keynames, "BASE", "COMPARE")))
    
}











