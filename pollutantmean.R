pollutantmean <- function(directory, pollutant, id = 1:332){
  getallfiles <- list.files(directory, full=TRUE)
  reducefiles <- getallfiles[id]
  parsefiledata <- lapply(reducefiles, read.csv, header=TRUE)
  combinefiledata <- do.call("rbind", parsefiledata)
  selectpollutant <- subset(combinefiledata, select = pollutant)
  getmean <- colMeans(selectpollutant, na.rm=TRUE)
  print(getmean)
}