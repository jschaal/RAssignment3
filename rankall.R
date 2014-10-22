rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    resultRowNames <- c("hospital","result","state")
    results <- data.frame()
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
    
    stateColumn <- 
        which(colnames(data) =="State" )
    
    hospitalColumn <- 
        which(colnames(data) =="Hospital.Name" )
    
    if (outcome == "heart attack") {
        colToUse <- heartAttackColumn
    }
    
    if (outcome == "heart failure") {
        colToUse <- heartFailureColumn
    }
    
    if (outcome == "pneumonia") {
        colToUse <- pneumoniaColumn
    }
    
    subdata <- data[,c(hospitalColumn,colToUse,stateColumn)]
    subdata[,2] <- suppressWarnings(as.numeric(subdata[,2]))
    names(subdata) <- resultRowNames
    
    for (state in state.abb) {
        #statedata <- subset(subdata,subdata$state==state)
        stateRecords <- subdata[,3] == state
        statedata <- subdata[stateRecords,]
        
        baddata <- is.na(statedata[,2]) 
        statedata <- statedata[!baddata,]
        statedata <-  statedata[order(statedata[,2],statedata[,1]),]  
        if (num == "best") {
            stateResults <- statedata[1,c(1,3)]
        } else if (num == "worst") {
            stateResults <- tail(statedata,1)[1,c(1,3)]
        } else
        {
            stateResults <- statedata[num,c(1,3)]
        }
        
        
        if (is.na(stateResults[1,2]))
        {
            #browser()
            stateResults <- data.frame("<NA>",state)
            names(stateResults) <- c("hospital","state")
        }
        results <- rbind(results,stateResults)
    }
    #browser()
    #    baddata <- is.na(results[,2]) 
    #    results  <- results[!baddata,c(1,3)]
    #results  <- results[,c(1,3)]
    #names(results) <- c("hospital","state")
    return(results)
    
}