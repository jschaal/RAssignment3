rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
 
    if (!(state %in%state.abb)) {
        stop("invalid state")
    }
    
    validCauses <- c("heart attack","heart failure","pneumonia")
    if (!(outcome %in% validCauses))
    {
        stop("invalid outcome")
    }
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    heartAttackColumn <- 
        which(colnames(data) =="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" )
    
    heartFailureColumn <- 
        which(colnames(data) =="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" )
    
    pneumoniaColumn <- 
        which(colnames(data) =="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
    data <- subset(data,data$State==state)
    
    colToUse <- heartAttackColumn
    
    if (outcome == "heart attack") {
        colToUse <- heartAttackColumn
    }
    
    if (outcome == "heart failure") {
        colToUse <- heartFailureColumn
    }
    
    if (outcome == "pneumonia") {
        colToUse <- pneumoniaColumn
    }
    
    subdata <- data[,c(2,colToUse)]
    subdata[,2] <- suppressWarnings(as.numeric(subdata[,2]))
    baddata <- is.na(subdata[,2]) 
    subdata <- subdata[!baddata,]
    subdata <-  subdata[order(subdata[,2],subdata[,1]),]  
    ranking <- rank(subdata[,2],ties.method = "first")
    subdata <- cbind(subdata,ranking)
    names(subdata) <- c("Hospital.Name","Rate","Rank")                
    if (num == "best") {
        return(subdata[1,1])
    }
    if (num == "worst") {
        return(tail(subdata,1)[1,1])
    }
    return(subdata[num,1])
    
}