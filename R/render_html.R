
render_html <- R6Class(
    "render_html", 
    inherit = render,
    public = list(

        file_content = function(){
            header <- add_tag( add_tag( diffdf_css(), "head"), "style")
            
            body <- add_tag(
                add_tag( self$strings, "div", args = "class='w-50'; style = 'margin: auto;'"),
                "body"
            )
            c(header, body)
        },
        
        file = function(filename){
            sink(filename)
            cat(self$file_content())
            sink()
        },
        
        display = function(){
            
            tempDir <- tempfile()
            dir.create(tempDir)
            htmlFile <- file.path(tempDir, "index.html")
            
            sink(htmlFile)
            cat(self$file_content())
            sink()
            
            viewer <- getOption("viewer")
            viewer(htmlFile)
        },
        
        h1 = function(x){
            add_tag(x, "h1")
        },
        
        h2 = function(x){
            add_tag(x, "h2")
        },
        
        h3 = function(x){
            add_tag(x, "h3")
        },
        
        h4 = function(x){
            add_tag(x, "h4")
        },
        
        p = function(x){
            add_tag(x, "p")
        },
        
        br = function(x){
            return("<br/>")
        },
        
        table = function(df, limitstring){
            
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
            
            add_tag( 
                c( 
                    add_tag(header_row, "thead", "class='thead-light'"), 
                    add_tag(dat_rows, "tbody"), 
                    caption
                ), 
                "table", 
                "class='table df-table table-striped'"
            )
        }
    
    )
)


add_tag <- function(x, tag, args = ""){
    
    if(args != "") args <- paste0(" ", args)

    x <- sprintf( 
        "<%s%s>%s</%s>", 
        tag, 
        args, 
        paste0(x, collapse=""),
        tag
    )
    return(x)
}


#' diffdf_css
#' 
#' Returns the CSS used by diffdf when rendering html output
#' 
#' @export
diffdf_css <- function(){
    cssbootfile <- system.file("css", "bootstrap.css", package = "diffdf")
    cssboot <- readLines(cssbootfile, warn = FALSE)
    
    csspkgfile <- system.file("css", "pkg.css", package = "diffdf")
    csspkg <- readLines(csspkgfile, warn = FALSE)
    
    css <- c(cssboot, csspkg)
    return(css)
}



