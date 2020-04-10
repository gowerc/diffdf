


add_tag <- function(x, tag, args = ""){
    paste0( "<" , tag,  " ", args,  " >" , paste0(x,collapse="") , "</", tag, ">")
}




html_viewer <- function(x){
    bootstrap_link <- '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">' 
    #bootstrap_link <- ""
    #css_file <- system.file("extdata", "bootstrap.css", package = "diffdf")
    #css <- readLines(css_file, warn = FALSE)
    #css <- add_tag( css, "style")
    header <- add_tag( bootstrap_link, "head")
    
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "index.html")
    sink(htmlFile)
    cat(header)
    cat(x)
    sink()
    viewer <- getOption("viewer")
    viewer(htmlFile)
}


#html_viewer(  render_html(iris))




render_html <- function (object, ...) {
    UseMethod("render_html", object)
}

render_html.issue <- function(object){
    
    
    #centre_div = "style = 'margin: auto; width: 50% !important;'"
    
    top <- add_tag( get_issue_message(object), "p")
    tab <- render_html(get_issue_value(object))
    div <- add_tag( c(top,tab), "div", args = "class='w-50'; style = 'margin: auto;'")
    
    paste0(div, "<br/>")
}


render_html.data.frame <- function(dat, rowlimit = 10){
    
    if( nrow(dat) > rowlimit){
        dat2 <- dat[seq_len(rowlimit),]
        caption <- add_tag( 
            paste0( "Showing " , rowlimit, " of ", nrow(dat), " observations"),
            "caption"
        )
    } else {
        dat2 <- dat
        caption <- ""
    }
    
    dat_char  <- apply(dat2, c(1, 2), as_cropped_char)
    dat_tag  <- apply(dat_char, c(1, 2), add_tag, tag = "td", args = "style='text-align: center;' class='df-cell'")
    dat_rows  <- apply(dat_tag, c(1), add_tag, tag = "tr class='df_row'")
    
    header <- sapply( names(dat2), add_tag , "th", args = "style='text-align: center;' class='df-header'")
    header_row <- add_tag( header , "tr", args = "class='df-row df_header-row'")
    
    add_tag( c( header_row, dat_rows, caption), "table", " class='table df-table'")
}



















