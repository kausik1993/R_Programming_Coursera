rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  
  if(! ( state %in% levels(factor(data$State)) ) ) {
    stop("invalid state")
  }
  
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  
  }
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  data = data[data$State==state,]
  data = data[,c(1,3,4,5)]
  if(outcome == "heart attack") {
    data = data[,c(1,2)]
  } else if(outcome == "heart failure") {
    data = data[,c(1,3)]
  } else if(outcome == "pneumonia") {
    data = data[,c(1,4)]
  }
  names(data)[2] = "Deaths"
  data[, 2] = suppressWarnings( as.numeric(data[, 2]) )
  data = data[!is.na(data$Deaths),]
  if(class(num) == "numeric" && num > nrow(data)){
    return (NA)
  }
  
  
  data = data[order(data$Deaths, data$Hospital.Name),]
  
  
  if(class(num) == "character") {
    if(num == "best") {
      return (data$Hospital.Name[1])
    }
    else if(num == "worst") {
      return (data$Hospital.Name[nrow(data)])
    }
  }
  else {
    return (data$Hospital.Name[num])
  }
}