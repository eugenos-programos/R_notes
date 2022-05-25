## description -- https://d3c33hcgiwev3.cloudfront.net/_2a0d228f9a48b3de85eedf022225fac9_ProgAssignment3.pdf?Expires=1653609600&Signature=I4ryuN9JmpVH5RTHjvah7~HOYqZahDFok2F~nPAcNRJcc9F7NOC1pXwF-x1UMldkQ-qoW9qqRuUheHBmstwyM1qbwcMelJZGzLKZZA0ZL6hUT8Cq85EXhTytQponMNaaeWG8QTjxwXmG5cCqO7Wh5v7gI0YjBdoNLuce7flJ66w_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A

best <- function(state, outcome){
    ## Read data
    data <- read.csv('../input/programming-assigment-4/outcome-of-care-measures.csv', colClasses = 'character')
    
    ## Check that state and outcome are valid
    valid_outcome_values <- c('heart attack', 'heart failure', 'pneumonia')
    if (!(outcome %in% valid_outcome_values)){
        message("Invalid outcome value")
        return 
    }
    valid_state_values = data$State
    if (!(state %in% valid_state_values)){
        message("Invalid state value")
        return
    }
    
    ## Find hospital name in that state with lowest 30-day death
    ## rate
    if (outcome == 'heart attack'){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    else if (outcome == 'heart failure'){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    only_state_data = data[data$State == state, ]
    only_state_data[outcome] = as.numeric(unlist(only_state_data[outcome]))
    only_state_data = only_state_data[!is.na(only_state_data[outcome]), ]
    only_state_data[only_state_data[outcome] == min(only_state_data[outcome]), ][1, 2]
}

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

rankhospital <- function(state, outcome, num = "best"){
    ## Read outcome data
    data <- read.csv('../input/programming-assigment-4/outcome-of-care-measures.csv', colClasses = 'character')
    
    ## Check that state and outcome are valid
    valid_outcome_values <- c('heart attack', 'heart failure', 'pneumonia')
    if (!(outcome %in% valid_outcome_values)){
        message("Invalid outcome value")
        return 
    }
    
    valid_state_values = data$State
    if (!(state %in% valid_state_values)){
        message("Invalid state value")
        return
    }
    if (outcome == 'heart attack'){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    else if (outcome == 'heart failure'){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else {
        outcome<- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    only_state_data = data[data$State == state, ]
    only_state_data[outcome] = as.numeric(unlist(only_state_data[outcome]))
    only_state_data = only_state_data[!is.na(only_state_data[outcome]), ]
    
    sorted_data = only_state_data[order(only_state_data[outcome]),]
    index = num
    if (num == "best"){
        index = 1
    }
    else if (num == "worst"){
        index = nrow(sorted_data)
    }
    sorted_data[index, 2]
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 9)
rankhospital("NY", "heart attack", 8)

rankall <- function(outcome, num = "best") {
  ## Read outcome data: COLS: HospitalName, State, HeartAttack, HearFailure, Pneumonia
  data <- read.csv("../input/programming-assigment-4/outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid  
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the (abbreviated) state name
  
  # Remove columns by outcome, only left HospitalName and Deaths by outcome
  if(outcome == "heart attack") {
    data = data[,c(1,2,3)]
  } else if(outcome == "heart failure") {
    data = data[,c(1,2,4)]
  } else if(outcome == "pneumonia") {
    data = data[,c(1,2,5)]
  }
  names(data)[3] = "Deaths"
  data[, 3] = suppressWarnings( as.numeric(data[, 3]) )
  
  # Remove rows with NA
  data = data[!is.na(data$Deaths),]
  
  splited = split(data, data$State)
  ans = lapply(splited, function(x, num) {
    # Order by Deaths and then HospitalName
    x = x[order(x$Deaths, x$Hospital.Name),]
    
    # Return
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
  
  
}

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
