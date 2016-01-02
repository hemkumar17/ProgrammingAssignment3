# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

best <- function(state, outcome){
  
  hospitalData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  valid.outcomes <- c("heart attack","heart failure","pneumonia")
  
  if (!state %in% hospitalData$State) {
    stop("invalid state")
  } else if(!outcome %in% valid.outcomes) {
    stop("invalid outcome")
  }
  
  Statedata <- subset(hospitalData, hospitalData$State==state)
  
  x <- subset(Statedata, select = c(Hospital.Name, State, 
                                    Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                    Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                                    Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  
  
  x[,3] <- suppressWarnings(as.numeric(x[,3]))
  x[,4] <- suppressWarnings(as.numeric(x[,4]))
  x[,5] <- suppressWarnings(as.numeric(x[,5]))

  if(outcome == "heart attack")
  {
    z <- subset(x, x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == 
                        min(na.omit(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
  }
  else if(outcome == "heart failure")
  {
    z <- subset(x, x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == 
                  min(na.omit(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
  }
  else if(outcome == "pneumonia")
  {
    z <- subset(x, x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == 
                  min(na.omit(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
  }
  
  as.vector(z[1,1])
  
}
