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



#' Progress bar printing
#' @usage for(i in 1:15){
#'   plot(1,1, main=i)
#'   Sys.sleep(1)
#'   progressbar(i, 15)
#'  }
#' @param curr.iter Current iteration value
#' @param tot.iter Number of total iterations
#' @param ini.iter Number of first iteration (in case loop does not start in 1. Default is 1.)
#' @param units units ("secs", "mins" or "hours" in which "ETC" should be calculated)
#' @param msg Character. Message to desplay above the progress bar.
#' @author Ignacio Ramos-Gutierrez
#' @export
progressbar <- function( curr.iter,tot.iter, ini.iter=1, units="mins", msg=NULL){

  curr.iter <- curr.iter - ini.iter +1
  tot.iter <- tot.iter - ini.iter +1
  if(units=="secs"){d <-0}else if(units=="hours"){d <- 2} else{d <- 1}

  if(curr.iter==1 & !is.null(msg)){cat(msg, "\n")}

  if(curr.iter==1){
    st <<- Sys.time()
    cat(paste0("0%       25%       50%       75%       100%", "\n",
               "|---------|---------|---------|---------|", "\n"))
  }

  v<- seq(from=0, to=40, by=40/tot.iter)
  v<- diff(ceiling(v))
  v <- cumsum(v)
  txt <- strrep("*", times=v[curr.iter])
  txt <- stringr::str_pad(txt, width = 45, side="right", pad=" ")
  ct <-Sys.time()
  et  <- as.numeric(difftime(ct, st, units=units))/curr.iter*(tot.iter-curr.iter)
  et <- round(et, digits=d)
  txt.end <- paste0(txt, "ETC: ", et, " ", units)
  if(curr.iter == ini.iter){txt.end <- paste0(txt, "ETC: ");maxnchar <<- nchar(txt.end)}
  if(curr.iter == tot.iter){txt.end <- paste0("*", txt, "DONE")}

  if(nchar(txt.end)>maxnchar){maxnchar <<- nchar(txt.end)}
  txt.end <- stringr::str_pad(txt.end, width = maxnchar, side="right", pad=" ")

  cat("\r")
  cat(txt.end)

  if(curr.iter == tot.iter){cat("\n")}
  if(curr.iter == tot.iter){ rm(list=c("st", "maxnchar"),envir =  .GlobalEnv)}
}



#' Move the mouse so the PC does not shut.
#'
#' @param use Logical. If TRUE, the function will barely move the mouse so the
#' user can keep using the PC.
#' @param days Number of days it should be running. note that days=1 (default)
#'        means that at 00:00 of the next day, the function will stop.
#' @param sleeptime time the system is slept between iterations
#'
#' @usage dontsleep()
#' @author Ignacio Ramos-Gutierrez
#' @export
dontsleep <- function(use=F, days=2, sleeptime=60){
  if(use){move<-1; t <- 0.01}else{move<-100; t <- 1}
  in.day <- Sys.Date()
  end.day <- in.day+days

  while(Sys.Date()!=end.day){
  coords <- KeyboardSimulator::mouse.get_cursor()

  KeyboardSimulator::mouse.move(coords[1]+move,coords[2]+move,t/2)
  KeyboardSimulator::mouse.move(coords[1],  coords[2],  t/2)
  Sys.sleep(sleeptime)
  }
}


#' Function to see if you are selected as Volunteer for the IBC
#'
#' @param ID Your ID used for the lookup
#'
#' @usage volunteeR()
#' @author Ignacio Ramos-Gutierrez
#' @export
volunteeR <- function(ID){
  require(gsheet)
  table <- as.data.frame(gsheet::gsheet2tbl("1jo1AklCzjFqOJfy51ihUskWSpKY6r9TMx3fm0Gls7rY"))
  l <- nchar(ID)
  ids <- vector("character")
  for (i in 1:(l-4)){
    id.i <- substr(ID, i, i+3)
    ids[i] <- paste0(paste0(rep("*", times=i-1), collapse = ""),
                     id.i,
                     paste0(rep("*", times=l-i-3), collapse = ""))
  }

  return( table[table[,2] %in% ids,1])
}


#' Function to set working directory where the script is
#'
#' @usage thiswd()
#' @usage setwd(thiswd())
#'
#' @author Ignacio Ramos-Gutierrez
#' @export
thiswd <- function(){
  return(dirname(rstudioapi::getSourceEditorContext()$path))
}
