rankhospital<-function(state,outcome,num="best"){
  input<-c("heart attack","heart failure","pneumonia")
  output<-c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  check<-data.frame(input,output)
  
  ## Read outcome data
  my_data<-read.csv("outcome-of-care-measures.csv",colClasses = "character") ## Reading the outcome data file

  ## Check that state and outcome are valid
  if(!state %in% my_data[,"State"])stop("invalid state")
  if(!outcome %in% check$input )stop("invalid outcome")
  check<-subset(check, check$input==outcome)
  myoutcome<-as.character(check$output)
  ## Return hospital name in that state with the given rank
  my_state<-subset(my_data,select=c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"),my_data$State == state)## subseting the data by state
  my_state<-subset(my_state,my_state[,myoutcome]!= "Not Available")
  my_state["rank"]<-myrank<-rank(as.numeric(as.character(my_state[,myoutcome])))
  ## 30-day death rate
  if(num=="best"){
    my_state<-subset(my_state,my_state$rank=="1")
    
  }
  else if (num=="worst"){
    mymax<-max(as.numeric(as.character(my_state[,"rank"])),na.rm=TRUE)
    my_state<-subset(my_state,my_state$rank==mymax)
  }
  else if (is.numeric(num)){
    my_state<-subset(my_state,my_state$rank==num)
  }
  my_state[,"Hospital.Name"]
}