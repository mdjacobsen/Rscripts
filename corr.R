corr <- function(directory, threshold=0){
  df <- complete(directory, 1:332)
  dfthreshold <- subset(df,nobs>threshold)
  fileids <- (subset(dfthreshold, select=id))
  fileidvector <- as.vector(fileids$id)
  getallfiles <- list.files(directory, full=TRUE)
  reducefiles <- getallfiles[fileidvector]
  correlateddata<-numeric()
  for (i in reducefiles){
    parsefiledata <- read.csv(i)
    cleandata <- na.omit(parsefiledata)
    nitratedata <- subset(cleandata, select=nitrate)
    nitratevector <- as.vector(nitratedata$nitrate)
    sulfatedata <- subset(cleandata, select=sulfate)
    sulfatevector <- as.vector(sulfatedata$sulfate)
    correlateddata <- c(correlateddata, cor(nitratevector,sulfatevector))
  }
  correlateddata
}