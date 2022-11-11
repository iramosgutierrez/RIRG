#R functions

#' Copy directory to R understanding format.
#' @param x String. File path copied from Windows' "copy file path".
#'
#' @return An R usable file path.
#'
#' @author Ignacio Ramos-Gutierrez
#' @examples
#' getpath(x = "C:\Users\User\Documents")
#' getpath()
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


#'  Merge table to attribute table in shapefile.
#' @usage joinAttributeTable(oldshape, table, oldshape$id, table$id)
#' @param x Shapefile where to add the table.
#' @param y Table to merge into the shapefile.
#' @param xcol column in shapefile with id names, which must match ycol values.
#' @param ycol column in table with id names, which must match xcol values.
#' 
#' @return A shapefile including the previous attribute table and the merged table.
#'
#' @author Ignacio Ramos-Gutierrez, copied from internet. https://stat.ethz.ch/pipermail/r-sig-geo/2014-July/021442.html
#'
#' @examples
#' newshape <- joinAttributeTable(oldshape, table, oldshape$id, table$id)
#' 
#' @export
joinAttributeTable <- function(x, y, xcol, ycol) {
  
  x$sort_id <- 1:nrow(as(x, "data.frame"))
  x.dat <- as(x, "data.frame") 
  x.dat2 <- merge(x.dat, y, by.x = xcol, by.y = ycol) 
  x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]
  x2 <- x[x$sort_id %in% x.dat2$sort_id, ]
  x2.dat <- as(x2, "data.frame")
  row.names(x.dat2.ord) <- row.names(x2.dat)
  x2@data <- x.dat2.ord
  return(x2)
}
