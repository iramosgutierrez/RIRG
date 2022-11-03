#R functions

#' Copy directory to R understanding format.
#'
#' @export
getfile <- function(x="clipboard"){
  x <- readClipboard(raw = F)
  x <- gsub("\\\"", "", x)
  x <- gsub("\\\\", "/", x)
  write(x, "clipboard")
  return(x)
}
