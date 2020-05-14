workingpath <- "C:\\Users\\pc\\Desktop\\Jihan"
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
    mean.pol <- vector()
    for(file.id in csv.list){
        file.path <- paste(directory, "\\", file.id, sep="")
        current.file <- read.csv(file.path, header=TRUE)
        mean.pol <- c(mean.pol, mean(current.file[pollutant][,1], na.rm=TRUE))
    }
    names(mean.pol) <- id
    return(mean.pol)
}

pollutantmean("specdata", "sulfate")
