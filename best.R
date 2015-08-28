
best <- function(state, outcome) {
  #Define good outcomes
  good <- c("heart attack","pneumonia","heart failure")
  #Exit if outcome parameter not valid
  if (!outcome %in% good) {
    stop("invalid outcome")
  }
  #Get appropriate Outcome column index so we don't have to deal with long colnames
  if (outcome == "heart attack") {
    colidx <- 11
  }
  else if (outcome == "pneumonia") {
    colidx <- 23
  }
  else {
    colidx <- 35
  }
  
  #Read in outcome data
  data <- read.csv("outcome-of-care-measures.csv",stringsAsFactor = FALSE, 
                   na.strings = "Not Available")[c(2,7,colidx)]
  
  #Perform state input validation
  if (!state %in% data[,"State"]) {
    stop("invalid state")
  }
  #Subset data by state parameter
  sub <- data[data$State == state,]
  #Remove NA values
  sub <- na.omit(sub)
  #Get lowest mortality rate
  low <- min(sub[,3])
  #Return name of hospital with lowest mortality rate
  return(sub[sub[,3] == low,1])
  
}

