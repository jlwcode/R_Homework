rankall <- function(outcome, num){
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that the outcome is valid and get data subset
  
  if (outcome == "heart attack") {
    request_data <- outcomes[,c(2,7,11)]
  } else if (outcome == "heart failure") {
    request_data <- outcomes[,c(2,7,17)]
  } else if (outcome == "pneumonia") {
    request_data <- outcomes[,c(2,7,23)]
  } else {
    stop("invalid outcome")
  }
  request_data[,3] <- sapply(request_data[,3],  as.numeric)
  ordered_data <- request_data[order(request_data[,2], request_data[,3], request_data[,1]),]
  states <- ordered_data[!duplicated(ordered_data[,2]),2]
  num_states <- length(states)

  df <- data.frame(hospital = character(num_states), state = character(num_states), row.names = states, 
                   stringsAsFactors=FALSE)
  for ( i in 1:num_states) {
    state <- states[i]
    state_data <- subset(ordered_data, ordered_data[,2] == state & ordered_data[,3] != "Not Available")
    num_hospitals <- nrow(state_data)
    if (num =="best") {
      rank <- 1
    } else if (num == "worst") {
      rank <- num_hospitals
    } else {rank = as.numeric(num) }
    hospital_name <- state_data[rank,1]
    df$hospital[i] <- hospital_name
    df$state[i] <- state
  }
  df
}

