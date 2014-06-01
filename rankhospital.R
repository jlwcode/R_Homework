rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid

  state_check <- which(sapply(outcomes[,7], function(x) any(x == state)))
  if (length(state_check) == 0) {
    stop("invalid state")
  }

  if (outcome == "heart attack") {
    request_data <- outcomes[,c(2,7,11)]
  } else if (outcome == "heart failure") {
    request_data <- outcomes[,c(2,7,17)]
  } else if (outcome == "pneumonia") {
    request_data <- outcomes[,c(2,7,23)]
  } else {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day
  ## mortality rate 

  state_data <- subset(request_data, request_data[,2] == state & request_data[,3] != "Not Available")
  state_data[,3] <- sapply(state_data[,3],  as.numeric)
  rate_ordered_data <- state_data[order(state_data[,3], state_data[,1]),]
  num_hospitals <- nrow(state_data)
  if (num =="best") {
    i <- 1
  } else if (num == "worst") {
    i <- num_hospitals
  } else i = as.numeric(num)
  hospital <- rate_ordered_data[i,1]
  hospital
}