


add_tag <- function(x, tag, args = ""){
    paste0( "<" , tag,  " ", args,  " >" , paste0(x,collapse="") , "</", tag, ">")
}




display_html <- function(x){
    bootstrap_link <- '
        <link rel="stylesheet" 
              href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" 
              integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" 
              crossorigin="anonymous"
        >
    ' 
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






render_check_html <- function(object){
    top <- add_tag( get_error_message(object), "p")
    tab <- render_df_html(get_value(object))
    div <- add_tag( c(top,tab), "div", args = "class='w-50'; style = 'margin: auto;'")
    paste0(div, "<br/>")
}




render_df_html <- function(df, rowlimit = 10){
    
    if( nrow(df) > rowlimit){
        df2 <- df[seq_len(rowlimit),]
        caption <- add_tag( 
            paste0( "Showing " , rowlimit, " of ", nrow(df), " observations"),
            "caption"
        )
    } else {
        df2 <- df
        caption <- ""
    }
    
    dat_char  <- apply(df2, c(1, 2), as_cropped_char)
    dat_tag  <- apply(dat_char, c(1, 2), add_tag, tag = "td", args = "style='text-align: center;' class='df-cell'")
    dat_rows  <- apply(dat_tag, c(1), add_tag, tag = "tr class='df_row'")
    
    header <- sapply( names(df2), add_tag , "th", args = "style='text-align: center;' class='df-header'")
    header_row <- add_tag( header , "tr", args = "class='df-row df_header-row'")
    
    add_tag( c( header_row, dat_rows, caption), "table", " class='table df-table'")
}



















