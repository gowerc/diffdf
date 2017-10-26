
#' Compares dataframes
#' @description  Emulates proc compare from SAS
#' Compares 2 data frames and outputs any differences.
#' @param base input dataframe
#' @param compare comparison dataframe
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' @param suppress_warnings Do you want to suppress warnings? (logical)
#' @param outfile Location and name of outputted file. Leave as NULL to not output a file
#' @import dplyr
#' @import stringr
#' @importFrom purrr set_names
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr pmap
#' @importFrom purrr map_dbl
#' @importFrom purrr map2_dbl
#' @importFrom purrr pmap_dbl
#' @importFrom purrr map_chr
#' @importFrom purrr map2_chr
#' @importFrom purrr pmap_chr
#' @importFrom purrr map_df
#' @importFrom purrr map2_df
#' @importFrom purrr pmap_df
#' @importFrom purrr map_lgl
#' @importFrom purrr map2_lgl
#' @importFrom purrr pmap_lgl
#' @importFrom purrr walk
#' @importFrom purrr walk2
#' @importFrom purrr pwalk
#' @examples
#' ## rcompare( AAE , QC_AAE , c("USUBJID" , "AESEQ"))
#' @export
rcompare <- function (base , compare , keys = NULL, suppress_warnings = F, outfile = NULL){
    
    BASE = base
    COMP = compare
    KEYS = keys
    SUPWARN = suppress_warnings
    
    ### Initatiate output object
    COMPARE <- list()
    class(COMPARE) <- c("rcompare" , "list") 
    
    
    ### If no key is suplied match values based upon row number
    if (is.null(KEYS)){
        BASE <- BASE %>% mutate( ..ROWNUMBER.. = row_number())
        COMP <- COMP %>% mutate( ..ROWNUMBER.. = row_number())
        KEYS  <- "..ROWNUMBER.."
    }
    
    
    #################
    #
    # Check essential variable properties (class & mode)
    # 
    
    COMPARE[["UnsupportedColsBase"]] <- identify_unsupported_cols(BASE) %>% 
        class_adder('rcompare_basic',
                    "There are columns in BASE with unsupported modes !!\n",
                    nrow, 1)
    COMPARE[["UnsupportedColsComp"]] <- identify_unsupported_cols(COMP) %>% 
        class_adder('rcompare_basic',
                    "There are columns in COMPARE with unsupported modes !!\n",
                    nrow, 2)
    
    
    COMPARE[["VarModeDiffs"]] <- identify_mode_differences(
        BASE = BASE, 
        COMP = COMP 
    ) %>% 
        class_adder('rcompare_basic',
                    "There are columns in BASE and COMPARE with different modes !!\n\n",
                    nrow, 3)
    
    COMPARE[["VarClassDiffs"]] <- identify_class_differences(
        BASE = BASE, 
        COMP = COMP
    ) %>% 
        class_adder('rcompare_basic',
                    "There are columns in BASE and COMPARE with different classes !!\n\n",
                    nrow, 4)
    
    exclude_cols <- c(
        COMPARE[["UnsupportedColsBase"]]$VARIABLE , 
        COMPARE[["UnsupportedColsComp"]]$VARIABLE,
        COMPARE[["VarClassDiffs"]]$VARIABLE,
        COMPARE[["VarModeDiffs"]]$VARIABLE
    )
    
    #################
    #
    # Check Validity of Keys
    # 
    
    
    
    BASE_key_count <- identify_properties(BASE) %>% 
        filter( VARIABLE %in% KEYS) %>% 
        nrow
    
    COMP_key_count <- identify_properties(COMP) %>% 
        filter( VARIABLE %in% KEYS) %>% 
        nrow
    
    if ( BASE_key_count != length(KEYS)  ){
        stop( "BASE is missing variables specified in KEYS")
    }
    
    if ( COMP_key_count != length(KEYS) ){
        stop( "COMP is missing variables specified in KEYS")
    }
    
    if( any(KEYS %in% exclude_cols)){
        stop("KEYS are either an invalid or contain different modes between BASE and COMP")
    }
    
    #################
    #
    # Check Attributes
    # 
    
    COMPARE[["AttribDiffs"]] <- identify_att_differences(
        BASE = BASE, 
        COMP = COMP , 
        exclude_cols = exclude_cols
    ) %>% 
        class_adder('rcompare_attrib',
                    "There are columns in BASE and COMPARE with differing attributes !!\n",
                    nrow, 5)
    
    COMPARE[["FactorlevelDiffs"]] <- COMPARE[["AttribDiffs"]] %>% 
        filter(ATTR_NAME == 'levels') %>% 
        select(-ATTR_NAME) %>% 
        class_adder('rcompare_attrib',
                    "There are columns in BASE and COMPARE with differing levels !!\n",
                    nrow, 6)
    
    
    COMPARE[["LabelDiffs"]] <- COMPARE[["AttribDiffs"]] %>% 
        filter(ATTR_NAME == 'label') %>% 
        select(-ATTR_NAME) %>% 
        class_adder('rcompare_attrib',
                    "There are columns in BASE and COMPARE with differing labels !!\n",
                    nrow, 7)
    
    #################
    #
    # Check data
    # 
    
    #  Check the provided by groups define unique rows
    if ( !has_unique_rows(BASE , KEYS) ){
        stop( "BY variables in BASE do not result in unique observations")
    }
    
    if ( !has_unique_rows(COMP , KEYS) ){
        stop( "BY variables in COMPARE do not result in unique observations")
    }
    
    BASE <- fix_factor_vars(BASE , KEYS)
    COMP <- fix_factor_vars(COMP , KEYS)
    
    COMPARE[["ExtRowsBase"]] <- identify_extra_rows(
        DS1 = BASE, 
        DS2 = COMP, 
        KEYS = KEYS
    ) %>% 
        class_adder('rcompare_basic',
                    "There are rows in BASE that are not in COMPARE !!\n",
                    nrow, 8)
    
    COMPARE[["ExtRowsComp"]] <- identify_extra_rows(
        DS1 = COMP, 
        DS2 = BASE, 
        KEYS = KEYS
    ) %>% 
        class_adder('rcompare_basic',
                    "There are rows in COMPARE that are not in BASE !!\n",
                    nrow, 9)
    
    COMPARE[["ExtColsBase"]] <- identify_extra_cols(
        DS1 = BASE, 
        DS2 = COMP
    ) %>% 
        class_adder('rcompare_basic',
                    "There are columns in BASE that are not in COMPARE !!\n",
                    nrow, 10)
    
    COMPARE[["ExtColsComp"]] <- identify_extra_cols(
        DS1 = COMP, 
        DS2 = BASE
    )%>% 
        class_adder('rcompare_basic',
                    "There are columns in COMPARE that are not in BASE !!\n",
                    nrow, 11)
    
    COMPARE[["VarDiffs"]] <- identify_differences(
        BASE = BASE, 
        COMP = COMP , 
        KEYS = KEYS, 
        exclude_cols = exclude_cols
    )%>% 
        class_adder('rcompare_list',
                    "The following columns are different between BASE and COMPARE\n",
                    checklength, 13 )
    
    
    ### Summarise the number of mismatching rows per variable
    if ( length(COMPARE[["VarDiffs"]]) ){
        COMPARE[["NumDiff"]] <- sapply( COMPARE[["VarDiffs"]] , nrow )
    } else {
        COMPARE[["NumDiff"]] <- 0
    }
    
    COMPARE[["NumDiff"]] <- class_adder(COMPARE[["NumDiff"]], 'rcompare_vector',
                                        "Not all values compared equal\n",
                                        sum, 12 )
    
    COMPARE[["Issues"]] <- check_for_issues( COMPARE, SUPWARN)
    
    if (!is.null(outfile)){
        produce_file(outfile, COMPARE)
    }
    
    return(COMPARE)
}






##### Example of use
# DF1 <- data.frame(
#     id = c(1,2,3,4,5,6),
#     v1 = letters[1:6],
#     v2 = c(NA , NA , 1 , 2 , 3 , NA)
# )
# 
# DF2 <- data.frame(
#     id = c(1,2,3,4,5,7),
#     v1 = letters[1:6],
#     v2 = c(NA , NA , 1 , 2 , NA , NA),
#     v3 = c(NA , NA , 1 , 2 , NA , 4)
# )
# rcompare(DF1 , DF1 , keys = "id")
# rcompare(DF1 , DF2 , keys = "id")
