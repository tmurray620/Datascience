rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome2 <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Create a dataframe with valid outcomes
  valid.outcomes <- data.frame(c("heart attack", "heart failure", "pneumonia"))
  
  ## Check for valid arguments
  
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
  state.hospital.outcome <- na.omit(state.hospital.outcome) 
  
  ## Sorting the data by state, then outcome and then hospital 
  sorted.data <- state.hospital.outcome[order(state.hospital.outcome$state.name, as.numeric(state.hospital.outcome[, 3]), state.hospital.outcome$hospital),]
  
  ## Create a list of the data split out by state
  list.by.state <- split(sorted.data, sorted.data$state.name)
  
##  hospital.list <- sapply(list.by.state, function(data) subset(data)[1, 2])
##  names(hospital.list)
  
  if(num == "best"){
    final.list <- sapply(list.by.state, function(data) subset(data)[1, 2])
    }
  if(num == "worst"){
    final.list <- sapply(list.by.state, function(data) subset(data)[nrow(data), 2]) 
    }  
  if(is.numeric(num)){
    final.list <- sapply(list.by.state, function(data) subset(data)[num, 2]) 
    }  
##  final.unlist <- unlist(final.list)
  
  final.output <- data.frame(hospital = final.list, state = names(final.list), row.names= names(final.list))
  final.output
  ##  tail(final.unlist)  
  
  
  
}



## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
