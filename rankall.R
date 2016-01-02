# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-
#   ing (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have

rankall <- function(outcome, num = "best"){
  
  hospitalData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  valid.outcomes <- c("heart attack","heart failure","pneumonia")
  
  if(!outcome %in% valid.outcomes) {
    stop("invalid outcome")
  }
  
  state <- unique(hospitalData$State)
  
  state <- sort(state)
  
  source("rankhospital.R")
  
  hospital <- c()
  
  for(eachstate in state)
  {
    hospital <- append(hospital,rankhospital(eachstate, outcome, num))
    
  }
  
  output <- data.frame(hospital, state)
  
}
