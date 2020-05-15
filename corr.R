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