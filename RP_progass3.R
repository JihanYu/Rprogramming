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
	if(num > n.hosp.state){ 
	  return(NA)
	}else if(num == "best"){
	  num <- 1
	}else if(num == "worst"){
	  num <- n.hosp.state
	}
	
	outcome.rank <- outcome.state[order(outcome.state[, disease.id]),]
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