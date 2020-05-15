# workingpath <- "C:\\Users\\pc\\Desktop\\Jihan"
workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\project"
setwd(workingpath)

pollutantmean <- function(directory, pollutant, id=1:332){
    csvfiles <- list.files(directory)
    n.csv <- length(id)
    make.filename <- function(num.id){
        if(num.id > 0 & num.id < 10){
            file.name <- paste("00", num.id, sep="")
        }else if(num.id >= 10 & num.id < 100){
            file.name <- paste("0", num.id, sep="")
        }else{
            file.name <- as.character(num.id)
        }
        file.name <- paste(file.name, ".csv", sep="")
        return(file.name)
    }
    csv.list <- sapply(id, make.filename)
    sum.pol <- 0; n.pol <- 0
    for(file.id in csv.list){
        file.path <- paste(directory, "\\", file.id, sep="")
        current.file <- read.csv(file.path, header=TRUE)
        sum.pol <- sum.pol + sum(current.file[pollutant][,1], na.rm=TRUE)
        n.pol <- n.pol + sum(!is.na(current.file[pollutant]))
    }
    return(sum.pol/n.pol)
}

pollutantmean("specdata", "sulfate")
pollutantmean("specdata", "sulfate", 1:10)  # 4.064128
pollutantmean("specdata", "nitrate", 70:72) # 1.706047
pollutantmean("specdata", "nitrate", 23) # 1.280833

complete <- function(directory, id=1:332){
	csvfiles <- list.files(directory)
    n.csv <- length(id)
    make.filename <- function(num.id){
        if(num.id > 0 & num.id < 10){
            file.name <- paste("00", num.id, sep="")
        }else if(num.id >= 10 & num.id < 100){
            file.name <- paste("0", num.id, sep="")
        }else{
            file.name <- as.character(num.id)
        }
        file.name <- paste(file.name, ".csv", sep="")
        return(file.name)
    }
    csv.list <- sapply(id, make.filename)
	nobs <- vector()
	for(file.id in csv.list){
		file.path <- paste(directory, "\\", file.id, sep="")
		current.file <- read.csv(file.path, header=TRUE)
		nobs <- c(nobs, sum(complete.cases(current.file)))
	}
	return(data.frame(id, nobs))
}

complete.cases

complete("specdata", 1)
##   id nobs
## 1  1  117

complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96

complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463

complete("specdata", 3)
##   id nobs
## 1  3  243

corr <- function(directory, threshold = 0){
    csvfiles <- list.files(directory)
    make.filename <- function(num.id){
        if(num.id > 0 & num.id < 10){
            file.name <- paste("00", num.id, sep="")
        }else if(num.id >= 10 & num.id < 100){
            file.name <- paste("0", num.id, sep="")
        }else{
            file.name <- as.character(num.id)
        }
        file.name <- paste(file.name, ".csv", sep="")
        return(file.name)
    }
    csv.list <- sapply(1:332, make.filename)	
	
	cor.id <- vector()	
	for(file.id in csv.list){
		file.path <- paste(directory, "\\", file.id, sep="")
		current.file <- read.csv(file.path, header=TRUE)
		if(sum(complete.cases(current.file)) > threshold){
			cor.id <- c(cor.id, cor(current.file$sulfate, current.file$nitrate, use="complete.obs"))
		}
	}
	
	return(cor.id)
}

source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814

summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313

cr <- corr("specdata", 400)
head(cr)
## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860

summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313

cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 

length(cr)
## [1] 0

cr <- corr("specdata")
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000

length(cr)
## [1] 323