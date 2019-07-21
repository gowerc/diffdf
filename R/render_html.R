
dat <- iris

add_tags <- function(x, tag, args = ""){
    paste0( "<" , tag,  " ", args,  " >" , paste0(x,collapse="") , "</", tag, ">")
}

render_html <- function(dat){
    dat2 <- dat
    dat2[]  <- apply(dat, c(1, 2), as_cropped_char)
    
    dat3 <- dat2
    dat3[]  <- apply(dat2, c(1, 2), add_tags, tag = "td", args = "style='text-align: right;'")
    
    dat4  <- apply(dat3, c(1), add_tags, tag = "tr")
    
    header <- sapply( names(dat), add_tags , "th", args = "style='text-align: right;'")
    header <- add_tags( header , "tr")
    
    add_tags( c( header, dat4), "table", " class='table'")
}


html_viewer <- function(x){
    
    css_file <- system.file("extdata", "bootstrap.css", package = "diffdf")
    css <- readLines(css_file, warn = FALSE)
    css <- add_tags( css, "style")
    
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "index.html")
    sink(htmlFile)
    cat(css)
    cat(x)
    sink()
    viewer <- getOption("viewer")
    viewer(htmlFile)
}


html_viewer(  render_html(iris))






























