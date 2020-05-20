workingpath <- "C:\\Users\\pc\\Desktop\\Jihan\\Rprogramming"
#workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\project\\Rprogramming"
setwd(workingpath)

read.outcome <- function(filename){
  library(stringr)
  
  outcome <- read.csv(filename, header=TRUE, colClasses="character")
  
  
  name.outcome <- names(outcome)
  death.id <- which(name.outcome %in% grep("^Hospital.30.Day.Death", name.outcome, value=TRUE))
  hosp.state.id <- which(name.outcome %in% c("Hospital.Name", "State"))
  outcome.id <- sort(c(hosp.state.id, death.id))
  
  outcome.death <- subset(outcome, select=outcome.id)
  outcome.death[, 3] <- as.numeric(outcome.death[,3])
  outcome.death[, 4] <- as.numeric(outcome.death[,4])
  outcome.death[, 5] <- as.numeric(outcome.death[,5])
  
  return(outcome.death)
}

best <- function(state, outcome) {
  options(warn = -1)
  ## Read outcome data
  outcome.data <- read.outcome("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  disease <- c("heart attack", "heart failure", "pneumonia")
  state.name <- names(table(outcome.data$State))
  
  if(!(state %in% state.name)){ stop("invalid state") }
  if(!(outcome %in% disease)){ stop("invalid outcome") }
  
  ## Return hospital name in that state with lowest 30-day death rate
  disease.id <- which(disease==outcome) + 2
  outcome.state <- outcome.data[outcome.data$State==state,]
  min.id <- which( outcome.state[,disease.id] == min(outcome.state[,disease.id], na.rm=TRUE) )
  best.hosp <- sort(outcome.state$Hospital.Name[min.id])[1]
  
  return(best.hosp)
}

source("best.R")
best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"

best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"

best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"

best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state

best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome


rankhospital <- function(state, outcome, num = "best") {
  options(warn = -1)
  ## Read outcome data
  outcome.data <- read.outcome("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  disease <- c("heart attack", "heart failure", "pneumonia")
  state.name <- names(table(outcome.data$State))
  
  if(!(state %in% state.name)){ stop("invalid state") }
  if(!(outcome %in% disease)){ stop("invalid outcome") }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  disease.id <- which(disease==outcome) + 2
  outcome.state <- outcome.data[outcome.data$State==state,]
  
  n.hosp.state <- nrow(outcome.state)
  n.state.na <- sum(is.na(outcome.state[, disease.id]))
  if(num == "best"){
    num <- 1
  }else if (num == "worst"){
    num <- n.hosp.state - n.state.na
  }
  if(is.numeric(num)){
    if(num > n.hosp.state){
      return (NA)
    }
  }else{
    stop("invalid number")
  }
  
  outcome.rank <- outcome.state[order(outcome.state[, disease.id], outcome.state[, 1]),]
  rank.hosp <- outcome.rank$Hospital.Name[num]
  
  return(rank.hosp)
}
	
source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
#[1] NA

rankall <- function(outcome, num = "best") {
  options(warn = -1)
  ## Read outcome data
  outcome.data <- read.outcome("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  disease <- c("heart attack", "heart failure", "pneumonia")
  state.name <- names(table(outcome.data$State))
  
  if(!(outcome %in% disease)){ stop("invalid outcome") }
  if( !(num %in% c("best", "worst")) & !(is.numeric(num)) ) { stop ("invalid number") } 

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  disease.id <- which(disease==outcome) + 2
  
  hospital <- vector()
  for(state.i in state.name){
	  outcome.state <- outcome.data[outcome.data$State==state.i,]
	  num.new <- num
	
	  n.hosp.state <- nrow(outcome.state)
	  n.state.na <- sum(is.na(outcome.state[, disease.id]))
	  if(num == "best"){
		  num.new <- 1
	  }else if(num == "worst"){
		  num.new <- n.hosp.state - n.state.na
	  }
	  if(num.new > n.hosp.state){
	    hospital <- c(hospital, NA)
	  }else{
	    outcome.rank <- outcome.state[order(outcome.state[, disease.id],
	                                        outcome.state[,1]),]
	    hospital <- c(hospital, outcome.rank$Hospital.Name[num.new])
	  }
  }  
	  
  rank.hosp.df <- data.frame(hospital, state = state.name)
  return(rank.hosp.df)
}

source("rankall.R")
head(rankall("heart attack", 20), 10)
#hospital state
#AK <NA> AK
#AL D W MCMILLAN MEMORIAL HOSPITAL AL
#AR ARKANSAS METHODIST MEDICAL CENTER AR
#AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
#CA SHERMAN OAKS HOSPITAL CA
#CO SKY RIDGE MEDICAL CENTER CO
#CT MIDSTATE MEDICAL CENTER CT
#DC <NA> DC
#DE <NA> DE
#FL SOUTH FLORIDA BAPTIST HOSPITAL FL

tail(rankall("pneumonia", "worst"), 3)
#hospital state
#WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
#WV PLATEAU MEDICAL CENTER WV
#WY NORTH BIG HORN HOSPITAL DISTRICT WY

tail(rankall("heart failure"), 10)
#hospital state
#TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
#TX FORT DUNCAN MEDICAL CENTER TX
#UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
#VA SENTARA POTOMAC HOSPITAL VA
#VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
#VT SPRINGFIELD HOSPITAL VT
#WA HARBORVIEW MEDICAL CENTER WA
#WI AURORA ST LUKES MEDICAL CENTER WI
#WV FAIRMONT GENERAL HOSPITAL WV
#WY CHEYENNE VA MEDICAL CENTER WY