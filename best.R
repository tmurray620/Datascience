best <- function(state, outcome) {
  
  ## Read outcome data
  outcome2 <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Create a dataframe with valid outcomes
  valid.outcomes <- data.frame(c("heart attack", "heart failure", "pneumonia"))
  
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
 
   ## Creating a data.frame with just the requested state
    outcomes.subset.by.state <- subset(outcome2, state.name == state)
    
  ## Determining which row / observance has the best (lowest) outcome  
    best.hospital <-which.min(outcomes.subset.by.state[,outcome])
    
  ## subset outcomes with the row from the previous step and column 2  
    outcomes.subset.by.state[best.hospital, 2]
    ## best.hospital["hospital"]
    
    
    ## head(outcomes.subset.by.state)
    
    ##data.subset <- outcome2[,c("hospital", 7, 11)]
  
  ## x.sub7 <- x.df[, c(1, 3, 5)]
  ## names(data.subset)
##  outcomes.subset.by.state <- subset(outcome2, "state" = state)
##  lowest.value <- which.min(outcomes.subset.by.state$outcome)
  
  
  ## Check that state and outcome are valid
     ## State = col 7
     ## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" "heart attack"
     ## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" "heart failure"
     ## "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"  "pneumonia"
  
    
  ## Return hospital name in that state with lowest 30-day death
  ## rate
     ## Hospital.Name = col 2
  
}
