#' Functions that do the initial processing on the REDCap data
#'  
#' 
#' STEP 1
#' Read in the data
#'  
#' @noRd
#' 


redcap_read_in <- function(export_forms = c("participant_access_request"),
                           date_field = "participant_access_request_timestamp"){
  
  #Basic readin with REDCap connection
  redcap_token <- Sys.getenv("REDCAP_SURVEY_API")
  redcap.conn <- redcapAPI::redcapConnection(url="https://redcap.dom.uab.edu/api/", token=redcap_token)
  redcap_curr <- redcapAPI::exportRecords(redcap.conn, factors=F, forms = export_forms)
  
  #Get meta-data, currently only using record_id
  #redcap_meta <- redcapAPI::exportRecords(redcap.conn, factors=F, forms="request_meta_data")
  
  return(redcap_curr)
  
}


redcap_logging <- function(rcon, ..., error_handling = getOption("redcap_error_handling")){
  
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_class(x = rcon, classes = "redcapApiConnection", 
                          add = coll)
  checkmate::assert_character(x = NULL, null.ok = TRUE, add = coll)
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"), add = coll)
  checkmate::reportAssertions(coll)
  body <- list(token = rcon$token, content = "log", format = "csv", 
               returnFormat = "csv")
  x <- httr::POST(url = rcon$url, body = body, config = rcon$config)
  if (x$status_code != 200) 
    return(redcap_error(x, error_handling))
  x <- utils::read.csv(text = as.character(x), stringsAsFactors = FALSE, 
                  na.strings = "")
  
}

#log_test <- redcap_logging(redcap.conn)




#This is the main function that steps through the survey sections to build the tables or long-form outputs

build_survey_tables <- function(.dat_orig, .id, id_col = "record_id",
                                dict_col = data_dict_col_names, dict_md = data_dict_md_names, dict_fact = data_dict_factor){
  
  #Catch to return NULL
  if(!(.id %in% .dat_orig[[id_col]])) return(NULL)
  
  #Initialize to get a single survey
  .dat <- .dat_orig[.dat_orig[[id_col]] == .id,]
  .out <- list()
  
  #Goals
  .out[["goal"]] <- long_form_builder(.dat, .sec = "goal")
  
  #PI
  .out[["pi"]] <- kable_builder(.dat, .sec = "pi")
  
  .out[["copi"]] <- kable_builder(.dat, .sec = "copi", .md_sec = "pi")
  
  
  #Details - hypothesis and aims
  .out[["details"]] <- long_form_builder(.dat, .sec = "details")
  .out[["theme"]] <- long_form_builder(.dat, .sec = "theme")
  
  #Funding
  .out[["funding"]] <- long_form_builder(.dat, .sec = "funding")
  
  #Sample Size
  .out[["sample_size"]] <- kable_builder(.dat, .sec = "sample_size", 
                                         added_var = c("partacc_target_pop_details"), 
                                         checkbox_ref_list = list(`partacc_target_pop` = c(1:5)), checkbox_ref_fill = "# Unlisted")
  
  #Minorities
  .out[["minorities"]] <- long_form_builder(.dat, .sec = "minorities")
  
  
  #The resources portion - we consider this in three parts: existing data, human subjects, biospecimens
  .out[["data_var"]] <- kable_builder(.dat, .sec = "data_var", added_var = "partacc_data_additional", dict_factor = data_dict_factor)
  .out[["human_subj"]] <- long_form_builder(.dat, .sec = "human_subj")
  
  #The biospecimen makes use of the unique biospec_builder function
  .out[["biospec"]] <- biospec_builder(.dat, .sec = c("bank_blood", "bank_other", "bank_cells", "bank_brain"))
  
  #And finally statistics
  .out[["stats"]] <- long_form_builder(.dat, .sec = "stats")
  
  return(.out)
}











#Specific function for building table-esque output e.g. principle investigator details

kable_builder <- function(.dat, .sec, .md_sec = NULL,
                          dict_col = data_dict_col_names, dict_md = data_dict_md_names, 
                          added_var = NULL, dict_factor = NULL, checkbox_ref_list = NULL, checkbox_ref_fill = NULL){
  
  #Subset the table based on the dictionary entry, pull the markdown names if they exist
  .curr <- .dat[,colnames(.dat) %in% dict_col[[.sec]]]
  #Reorder if necessary
  .curr <- .curr[,match(colnames(.curr), dict_col[[.sec]])]
  
  
  #Bunch of catches depending on whether we're passing markdown names {is.null(dict_md)} or using an alternate entry in the dictionary {!is.null(.md_sec)}
  if(is.null(dict_md)){
    .curr_md <- ""
  } else {
    if(!is.null(.md_sec)){
      .curr_md <- dict_md[[.md_sec]]
    } else{
    .curr_md <- dict_md[[.sec]]
    }
  }
  
  
  #Next check for factor recasts as needed
  #This is functionally similar to the function in long_form_builder but we a hard coded entry since we're recasting several examples of the same file use .sec
  if(!is.null(dict_factor)){
    
    #I'm using an sapply here instead of a for and it's frankly causing more issues than it's really worth
    #It coerces my single row data.frame into a vector and if I use a do.call after a lapply I lose the column names I'm indexing off of
    #As a results I have to do things like use `length(.curr)` and `names(.curr)` which still work on the single row data.frame but this was really a bit of a hassle
    .curr <- sapply(colnames(.curr), function(.col){
      if(grepl(dict_factor[[.sec]][["head"]], .col)){
        dict_factor[[.sec]][["levels"]][.curr[[.col]]]
      } else .curr[[.col]]
    })
  }
  
  
  #Build the table from the transpose vector along with markdown names
  if(is.data.frame(.curr)){  .curr <- data.frame(cols = t(.curr), md = .curr_md)
  } else .curr <- data.frame(cols = .curr, md = .curr_md)
  
  #Build column and rownames as needed
  colnames(.curr) <- c("cols", "md")
  if(.curr_md[1] == ""){ rownames(.curr) <- NULL
  } else{
    rownames(.curr) <- .curr_md
  }
  
  #We have a slight catch here for the sample size which is a unique combination of check boxes leading to text boxes
  #Essentially we use the list in checkbox_ref_list to see if a check box for a variable has been ticked even if the associated variable is empty
  if(!is.null(checkbox_ref_list)){
    
    #Get the REDCap checkbox columns
    checkbox_cols <- paste0(names(checkbox_ref_list), "___", checkbox_ref_list[[1]])
    checkbox_vector <- .dat[,colnames(.dat) %in% checkbox_cols]
    
    #Get any that equal 1 (i.e. they were checked)
    checkbox_vector <- checkbox_vector[,checkbox_vector == 1,drop=FALSE]
    
    #Adjust the column names so they align with rows of the table
    #This does mean there can't be any gaps in the table 
    #  e.g. the total summation for sample size is at the end which is fine even though it's not part of the checkbox list
    colnames(checkbox_vector) <- gsub(paste0(names(checkbox_ref_list), "___"), "", colnames(checkbox_vector))
    
    #Only include checkboxes that DON'T have a value already in "cols"
    check_to_fill <- which(is.na((.curr$cols[as.numeric(colnames(checkbox_vector))])))
    check_to_fill <- which(is.na((.curr$cols[as.numeric(colnames(checkbox_vector))])))
    #if(length(check_to_fill) > 0) checkbox_vector <- checkbox_vector[,-check_to_fill]
    
    #If there are any checkboxes still marked, fill in those portions of the table
    if(nrow(checkbox_vector) > 0) {
      .curr <- .curr[as.numeric(colnames(checkbox_vector)),]
      if(length(check_to_fill) > 0) .curr$cols[check_to_fill] <- checkbox_ref_fill
    }
  }
  
  
  
  #Drop rows with NA/blank 
  row_drop <- which(is.na(.curr$cols) | .curr$cols == "")
  if(length(row_drop) > 0) .curr <- .curr[-row_drop, ]
  
  
  #Regex catch for escaping things like parenthesis for phone numbers
  #We have two versions here since gsub recasts the dataframe into a vector
  #This is a hold over when we applied this to .curr the vector instead of .curr$cols
  #  It's also the reason we don't need to do `cols = t(.curr)` when building the table below
  #We now move this below since it casts raw NA's as a character
  #.curr$cols <- gsub("(\\(|\\))", paste0("\\\\", "\\1"), .curr$cols)
  .curr$cols <- gsub("^(\\(.*)", paste0("<span>", "\\1", "</span>"), .curr$cols)
  #.curr <- sapply(.curr, gsub, pattern = "(\\(|\\))", replacement = paste0("\\\\", "\\1"), x = .curr)

  #Create a new column of concatenated strings, not being used right now but we're keeping the code in
  if(nrow(.curr) > 0) .curr$string <- paste0(.curr$md, ": ", .curr$cols)
  
  
  
  
  
  
  #Create a null Kable if the table is empty
  if(nrow(.curr) == 0) {
    .curr_kab <- NULL
  
  #Otherwise, build the usual kable
  } else {
    .curr_kab <- .curr[colnames(.curr) %in% c("cols")]
  
    .curr_kab <- kableExtra::kbl(.curr_kab, "html", 
                       align=c("l", rep("c", ncol(.curr_kab))),  
                       col.names=NULL,      
                       escape=FALSE, 
                       linesep="")
    
    .curr_kab <- kableExtra::kable_styling(.curr_kab, font_size=24, full_width=F, position = "left")
  }
  
  
  #Build the list
  .out <- list(dat = .curr, kable = .curr_kab)
  
  #Section to add any info from additional variables that get passed
  if(!is.null(added_var) && length(.dat[,colnames(.dat) %in% added_var]) >0 ){
    extra <- .dat[,colnames(.dat) %in% added_var]
    names(extra) <- added_var
    .out[["extra"]] <- extra
  }
  
  #Return the list
  return(.out)
  
  
}







#Function for building long-form output akin to a paragraph

long_form_builder <- function(.dat, .sec, .md_sec = NULL,
                              dict_col = data_dict_col_names, dict_md = data_dict_md_names, dict_factor = data_dict_factor){
  
  #Subset the table based on the dictionary entry, pull the markdown names if they exist
  .curr <- .dat[,colnames(.dat) %in% dict_col[[.sec]]]
  #Reorder if necessary
  .curr <- .curr[,match(colnames(.curr), dict_col[[.sec]])]
  
  #Not going to worry about the md catches for now, may need to write in later
  #Do need the row drops although know its a column drop
  #Drop colss with NA/blank 
  col_drop <- which(is.na(.curr) | as.character(.curr) == "")
  if(length(col_drop) > 0) .curr <- .curr[-col_drop]
  
  #Next check for factor recasts as needed
  if(sum(colnames(.curr) %in% names(dict_factor)) > 0){
    
    #I'm using an sapply here instead of a for and it's frankly causing more issues than it's really worth
    #It coerces my single row data.frame into a vector and if I use a do.call after a lapply I lose the column names I'm indexing off of
    #As a results I have to do things like use `length(.curr)` and `names(.curr)` which still work on the single row data.frame but this was really a bit of a hassle
    .curr <- sapply(colnames(.curr), function(.col){
      if(.col %in% names(dict_factor)){
        dict_factor[[.col]][.curr[[.col]]]
      } else as.character(.curr[[.col]])
    })
  }
  
  #Pull the column names to index the MD dictionary, just using names() instead of colnames() since we may get a vector after factor recasts
  curr_cols <- names(.curr)
  
  #Default missing message is no columns remain, we use length since all our dataframes will be a row and the factor recast coerces to a vector anyway
  if(length(.curr) == 0) {.curr_out <- "No material listed in survey"
  
  #Otherwise, regex some line breaks and print simple output with the indexed MD names and the written text
  } else {
    .curr <- gsub("\\n", "<br>", .curr)
    names(.curr) <- curr_cols
    
    #Build the final output list
    .curr_out <- list(names = dict_md[[.sec]][which(dict_col[[.sec]] %in% curr_cols)], string = .curr)
  }
  
  #Return the list
  list(dat = .curr, form = .curr_out)
  
}




#A special function for building the biospecimen tables since they use check boxes and linked files
#It expects .sec as a vector

biospec_builder <- function(.dat, .sec,
                            dict_col = data_dict_col_names, dict_md = data_dict_md_names,
                            dict_groups = data_dict_group_names, 
                            link_string = "_vol", link_to_replace = "___1"){
  
  #Unlike the other builders we're coercing this to a list format
  .out <- 
    lapply(.sec, function(sec_curr){
      
      #First do the usual dataset pull and reorder
      .curr <- .dat[,colnames(.dat) %in% dict_col[[sec_curr]]]
      .curr <- .curr[,match(colnames(.curr), dict_col[[sec_curr]])]
      
      #MD names will always be required, don't bother with a catch
      .curr_md <- dict_md[[sec_curr]]
      
      #Get the name of the list
      .curr_name <- dict_groups[[sec_curr]]
      
      #Build the dataframe similar to the kable_builder but with some extra columns
      .curr <- data.frame(cols = t(.curr), md = .curr_md, var_names = colnames(.curr))
      colnames(.curr) <- c("cols", "md", "var_names")
      
      #Drop any column that are missing a value in cols (e.g. NA or a 0 from a checkbox)
      row_drop <- which(is.na(.curr$cols) | .curr$cols==0)
      if(length(row_drop) > 0) .curr <- .curr[-row_drop,]
      
      #If nothing is left in the table, return NULL
      if(nrow(.curr) == 0) return(list(name = .curr_name, kable = NULL))
      
      #Next up, we prep the string to print depending on whether there are link values to check
      
      #Initialize the string
      .curr$string <- .curr$md
      
      #Now step through the variable names, gsub out the link string, check if it exists, and tack on the link to the string
      for(ii in seq_len(nrow(.curr))){
        link_check <- gsub(link_to_replace, link_string, .curr$var_names[ii])
        if(!is.null(.dat[[link_check]]) && !is.na(.dat[[link_check]])){
          .curr$string[ii] <- paste0(.curr$string[ii], " (", .dat[[link_check]], ")")
        }
      }
      
      #Now we're ready to build the kable
      .curr_kab <- data.frame(V1 = .curr$string)
      rownames(.curr_kab) <- NULL
      .curr_kab <- kableExtra::kbl(.curr_kab, "html", 
                                   align=c("l", rep("c", ncol(.curr_kab))),  
                                   col.names=NULL,      
                                   escape=FALSE, 
                                   linesep="")
      
      .curr_kab <- kableExtra::kable_styling(.curr_kab, font_size=18, full_width=F, position = "left") 
      
      #Finally, return the final list
      return(list(name = .curr_name, kable = .curr_kab))
      
    })
  
  #Quickly make sure there's at least one kable
  kable_check <- lapply(.out, function(.check){
    if(!is.null(.check$kable)) {1
      } else 0
  })
  kable_check <- do.call(sum, kable_check)
  if(kable_check == 0) return(NULL)
  
  #Return the final processed list, we'll build this out in the markdown portion
  return(.out)
  
  
}



