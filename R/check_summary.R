
get_diffdf_summary <- function(base, comp, keys, base_name, comp_name, COMPARE){
    
    
    class_base <- paste(class(base), collapse = ", ")
    class_comp <- paste(class(comp), collapse = ", ")
    
    df_summary <- construct_check(
        value = data.frame(
            "Summary" = c("Number of rows", "Number of columns", "Class"),
            "BASE" = c( nrow(base), ncol(base), as_cropped_char(class_base)),
            "COMPARE" = c( nrow(comp), ncol(comp), as_cropped_char(class_comp)),
            stringsAsFactors = FALSE
        ),
        error_message = "DataFrame Summary",
        was_performed = FALSE,
        print_me = TRUE
    )
    
    key_list <- construct_check(
        value = data.frame(
            "Variable" = KEYS,
            stringsAsFactors = FALSE
        ),
        error_message = "Listing of Keys",
        was_performed = FALSE,
        print_me = TRUE
    )
    
    if( !diffdf_has_issues(COMPARE)){
        tail_check <- construct_check(
            value = data.frame(),
            error_message = "No issues found!",
            was_performed = FALSE,
            print_me = TRUE
        )
    } else {
        tail_check <- construct_check(
            value = data.frame(),
            error_message = "Differences were found, details are provided below:",
            was_performed = FALSE,
            print_me = TRUE
        )
    }
    
    construct_check_collection(
        is_test = FALSE,
        print_me = TRUE,
        ord = 1,
        error_message = paste0(
            "Comparison of ", base_name, " (BASE) vs ", comp_name, " (COMPARE)",
            collapse = ""
        ), 
        value = data.frame(),
        collection = list(df_summary, key_list, tail_check)
    )
    
}
