complete<- function(directory, id = 1:332){
  getallfiles <- list.files(directory, full=TRUE)
  reducefiles <- getallfiles[id]
  idindex <- 1
  df<-data.frame(id=numeric(), nobs=numeric())
  for (i in reducefiles) {
    parsefiledata<-read.csv(i)
    cleandata <- na.omit(parsefiledata)
    countrows <- nrow(cleandata)
    newrow = c(id[idindex],countrows)
    df = rbind(df,newrow)
    idindex <- idindex + 1
  }
  colnames(df)<-c('id', 'nobs')
  df
}