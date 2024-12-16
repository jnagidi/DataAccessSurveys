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