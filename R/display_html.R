
add_tag <- function(x, tag, args = ""){
    paste0( "<" , tag,  " ", args,  " >" , paste0(x,collapse="") , "</", tag, ">")
}




render_html <- function(x){
    
    cssbootfile <- system.file("css", "bootstrap.min.css", package = "diffdf")
    cssboot <- readLines(cssbootfile, warn = FALSE)
    
    csspkgfile <- system.file("css", "pkg.css", package = "diffdf")
    csspkg <- readLines(csspkgfile, warn = FALSE)
    
    css <- c(cssboot, csspkg)
    
    header <- add_tag( add_tag( css, "head"), "style")
    
    body <- add_tag(
        add_tag( x, "div", args = "class='w-50'; style = 'margin: auto;'"),
        "body"
    )
    
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "index.html")
    sink(htmlFile)
    cat(header)
    cat(gsub( "\n", "<br/>", body))
    sink()
    viewer <- getOption("viewer")
    viewer(htmlFile)
}



as_html_title <- function(x){
    add_tag(x, "h4")
}

as_html_string <- function(x){
    add_tag(x, "p")
}

as_html_table <- function(df, limitstring){
    
    if( !is.na(limitstring)){
        caption <- add_tag(limitstring, "caption")
    } else {
        caption <- ""
    }
    
    dat_char  <- apply(df, c(1, 2), as_cropped_char)
    dat_tag  <- apply(dat_char, c(1, 2), add_tag, tag = "td", args = "style='text-align: center;' class='df-cell'")
    dat_rows  <- apply(dat_tag, c(1), add_tag, tag = "tr class='df_row'")
    
    header <- sapply( names(df), add_tag , "th", args = "style='text-align: center;' class='df-header'")
    header_row <- add_tag( header , "tr", args = "class='df-row df_header-row'")
    
    add_tag( c( header_row, dat_rows, caption), "table", " class='table df-table'")
}


