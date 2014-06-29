rankall<-function(outcome, num){
  outcomecolumnid<-c(11,17,23)
  outcomenames<-c("heart attack","heart failure","pneumonia")
  outcomes<-data.frame(outcomecolumnid,outcomenames)
  colnames(outcomes)<-c("outcomecolumnid","outcomenames")
  if (!(outcome %in% outcomes$outcomenames)) {
    stop("invalid outcome")
  }
  outcomesubset<-subset(outcomes,outcomenames==outcome)
  outcomeid<-outcomesubset$outcomecolumnid
  qualitydata<-(read.csv("outcome-of-care-measures.csv", colClasses="character"))
  outcomedata<-na.omit(qualitydata[, c(2,7,outcomeid)])
  colnames(outcomedata)<-c("HospitalName", "State", "Outcome")
  numericoutcomedata<-transform(outcomedata, Outcome = as.numeric(Outcome))
  cleanoutcomedata<-na.omit(numericoutcomedata)
  states<-sort(unique(cleanoutcomedata$State))
  df<-data.frame("HospitalName"=character(), "State"=character())
  for (i in states){
    HospitalName<-rankhospital(i,outcome,num)
    newrow = data.frame(HospitalName, i)
    df = rbind(df,newrow)
    }
  colnames(df)<-c('hospital', 'state')
  df
}
  