rankhospital <- function(state, outcome, num = "best") {
 
   ## Read outcome data
  outcome2 <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Create a dataframe with valid outcomes
  valid.outcomes <- data.frame(c("heart attack", "heart failure", "pneumonia"))
  
  ## Check for valid arguments
  if (any(outcome2["State"] == state, na.rm = TRUE) != TRUE)
    
    stop(print("state is invalid"))
  
  if(any(valid.outcomes[1] == outcome, na.rm = TRUE) != TRUE)
    
    stop(print("outcome is invalid"))
  
  ## Renaming the key columns. Has to be a better way to do this but...
  names(outcome2)[2] <- "hospital"
  names(outcome2)[7] <- "state.name"
  names(outcome2)[11] <- "heart attack"
  names(outcome2)[17] <- "heart failure"
  names(outcome2)[23] <- "pneumonia"
  
  ## Creating 3 vectors based on arguments
  state.vector <- outcome2[7]
  hospital.vector <- outcome2[2]
  outcome.vector <- outcome2[outcome]
  
  ## Creating a new data frame from the vectors
  state.hospital.outcome <- cbind.data.frame(c(state.vector, hospital.vector, outcome.vector), stringsAsFactors = FALSE)
  
  ## Creating a data.frame with just the requested state
  outcomes.subset.by.state <- subset(state.hospital.outcome, state.name == state)
  
  ## Sorting the data by outcome and then hospital 
  sorted.data <- outcomes.subset.by.state[order(as.numeric(outcomes.subset.by.state[, 3]), outcomes.subset.by.state$hospital),]
  tail(sorted.data)
  
  ## Removing NAs
  sorted.data <- na.omit(sorted.data)

if(num == "best"){ num <- 1}

if(num == "worst"){ num <- nrow(sorted.data)}

sorted.data[num, 2]
  
}
  
 
