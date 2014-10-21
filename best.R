best <- function(state, outcome) {
    
    outData <- NULL
    if (!(state %in%state.abb)) {
        stop("invalid state")
    }
    
    validCauses <- c("heart attack","heart failure","pneumonia")
    if (!(outcome %in% validCauses))
    {
        stop("invalid outcome")
    }
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    data <- subset(data,data$State==state)
    data <-  data[order(data$Hospital.Name),]
    
    if (outcome == "heart attack")
    {
        data[,11] <- suppressWarnings(as.numeric(data[,11]))
        mindeaths = min(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ,
                        na.rm = T)
        heartsubset <- subset(data,
                              data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 
                              == mindeaths)
        bestHospital <- heartsubset[1,2]
        return(bestHospital)
        
    }
 
    if (outcome == "heart failure")
    {
        data[,17] <- suppressWarnings(as.numeric(data[,17]))
        mindeaths = min(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ,
                        na.rm = T)
        heartsubset <- subset(data,
                              data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure 
                              == mindeaths)
        bestHospital <- heartsubset[1,2]
        return(bestHospital)
        
    }
    
    if (outcome == "pneumonia")
    {
        data[,23] <- suppressWarnings(as.numeric(data[,23]))
        mindeaths = min(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ,
                        na.rm = T)
        heartsubset <- subset(data,
                              data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia 
                              == mindeaths)
        bestHospital <- heartsubset[1,2]
        return(bestHospital)
        
    }
    
    
}