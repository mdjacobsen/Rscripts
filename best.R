best <- function(state, outcome) {
  states<-read.table("States.txt", col.names="State")
  outcomecolumnid<-c(11,17,23)
  outcomenames<-c("heart attack","heart failure","pneumonia")
  outcomes<-data.frame(outcomecolumnid,outcomenames)
  colnames(outcomes)<-c("outcomecolumnid","outcomenames")
    if (!(state %in% states$State)) {
      stop("invalid state")
    }
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
  filteredoutcomedata<-subset(cleanoutcomedata, State == state)
  orderedoutcomedata<-filteredoutcomedata[ order(filteredoutcomedata[,3],filteredoutcomedata[,1]),]
  best<-subset(orderedoutcomedata,select=HospitalName)
  finalbest<-as.vector(best$HospitalName)
  finalbest[1]
}
