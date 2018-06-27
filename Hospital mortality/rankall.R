rankall <- function(outcome, num="best") {
  
  outcome.names <- c("heart attack", "heart failure", "pneumonia")
  
  # check validity of outcome
  if (!outcome %in% outcome.names) {
    stop("invalid outcome")
  }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  # rename outcome columns for easy reference
  names(data)[c(11,17,23)] <- outcome.names
  data <- data[,c("State","Hospital.Name",outcome)]
  data[,outcome] <- suppressWarnings(as.numeric(data[,outcome]))
  data <- data[!is.na(data[outcome]),]
  data <- data[order(data$State, data[outcome], data$Hospital.Name),]
  ranksbystate <- aggregate(data, by=list(data$State), function(x) {
    if (!is.numeric(num)) {
      if (num == "best") {
        num <- 1
      } else if (num == "worst") {
        num <- length(x)
      } else {
        stop("invalid num")
      } 
    }
    x[num]
  })
  out <- ranksbystate[,c(3,1)]
  names(out) <- c("hospital","state")
  
  return(out)
}