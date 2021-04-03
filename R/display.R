
as_display_item <- function(x, type){
    y <- list(
        content = x,
        type = type
    )
    class(y) <- c("display_item")
    return(y)
}


#' @export
d_h1 <- function(x) as_display_item(x, "h1")


#' @export
d_h2 <- function(x) as_display_item(x, "h2")


#' @export
d_h3 <- function(x) as_display_item(x, "h3")


#' @export
d_h4 <- function(x) as_display_item(x, "h4")


#' @export
d_br <- function(x="") as_display_item(x, "br")


#' @export
d_p <- function(x) as_display_item(x, "p")


#' @export
d_table <- function(x) as_display_item(x, "table")


is_display <- function(x){
    cond1 <- length(class(x)) == 1
    cond2 <- all(class(x) %in% c("display"))
    return(cond1 & cond2)
}


#' @export
flatten_display <- function(x){
    stopifnot(is_display(x))
    HOLD <- list()
    INDEX <- 1
    for(i in seq_along(x)){
        y <- x[[i]]
        if("display_item" %in% class(y)){
            HOLD[[INDEX]] <- y
            INDEX <- INDEX + 1
        } else if( is_display(y)){
            HOLD <- append(HOLD, flatten_display(y))
        } else {
            stop("Object is not a display_item nor a display")
        }
    }
    return(as_display(HOLD))
}


#' @export
as_display <- function(x){
    class(x) <- "display"
    return(x)
}


#' @export
display <- function(...){
    x <- list(...)
    
    for(i in x){
        stopifnot( all(class(i) %in% c("display", "display_item")))
    }
    
    return(as_display(x))
}



#' @export
print.display <- function(
    x,
    type = "ascii", 
    rowlimit = 10, 
    file = NULL, 
    display = is.null(file)
){
    if( all(class(type) == "R6ClassGenerator")){
        renderer <- type
    } else if( type == "ascii"){
        renderer <- render_ascii
    } else if( type == "html"){
        renderer <- render_html
    } else {
        stop("Invalid type")
    }
    
    rend <- renderer$new(
        x,
        rowlimit = rowlimit
    )
    if(display) rend$display()
    if(!is.null(file)) render$file(file)
    return(invisible(rend$strings))
    
}


#' @export
#' @rdname print.diffResult
as.character.displays <- function(x, ...){
    x <- print(x, display = FALSE, ...)
    return(x)
}

