best<-function(state,outcome){
   ##Read outcome data
  input<-c("heart attack","heart failure","pneumonia")
  output<-c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  check<-data.frame(input,output)
  check<-subset(check, check$input==outcome)
  if(nrow(check)<1)stop("invalid outcome")
  myoutcome<-as.character(check$output)
  my_data<-read.csv("outcome-of-care-measures.csv",colClasses = "character") ## Reading the outcome data file
  ##check that state and outcome are valid
  my_state<-subset(my_data,select=c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"),my_data$State == state)## subseting the data by state
  if(nrow(my_state)<1) stop("invalid state")  ## checking to valid the state
 ## myoutcome<-gsub(outcome,pattern = " ", replacement = "\\.")
##  myoutcome<-paste0("Hospital.30.Day.Death..Mortality..Rates.from.",myoutcome)
  my_state<-subset(my_state,my_state[,myoutcome]!= "Not Available")
  mymin<-min(as.numeric(as.character(my_state[,myoutcome])),na.rm=TRUE)
  ##Return hospital name in that state with lowest 30-day death rate
 my_state<-subset(my_state,as.numeric(as.character(my_state[,myoutcome]))==mymin)
 my_state[order(my_state[,"Hospital.Name"]),"Hospital.Name"]
 
  }