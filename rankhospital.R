# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num){
  
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
    z <- na.omit(x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, x$Hospital.Name),])
  }
  else if(outcome == "heart failure")
  {
    z <- na.omit(x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, x$Hospital.Name),])
  }
  else if(outcome == "pneumonia")
  {
    z <- na.omit(x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, x$Hospital.Name),])
  }
  
  if(num == "best") num = 1
  else if(num == "worst") num = nrow(z)

  as.vector(z[num,1])
}
