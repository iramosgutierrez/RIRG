#R functions

#' Copy directory to R understanding format.
#'
#' @export
getpath <- function(x="clipboard"){
  x <- readClipboard(raw = F)
  x <- gsub("\\\"", "", x)
  x <- gsub("\\\\", "/", x)
  write(x, "clipboard")
  return(x)
}

#' getting non-NA values
#' @export
notNA <- function(x){
  return(x[!(is.na(x))])
}


#' @export
firstword <- function(x){
  return(sapply(stringr::str_extract_all(x, "[A-Za-z\\-\\.]+"), `[`, 1))
  
}
