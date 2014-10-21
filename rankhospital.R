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

        if (outcome == "heart attack")
        {
                
                subdata <- data[,c(2,11)]
                subdata[,2] <- suppressWarnings(as.numeric(subdata[,2]))
                baddata <- is.na(subdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) 
                subdata <- subdata[!baddata,]
                subdata <-  subdata[order(subdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                    subdata$Hospital.Name),]    
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

        
        if (outcome == "heart failure")
        {
                
                subdata <- data[,c(2,11)]
                subdata[,2] <- suppressWarnings(as.numeric(subdata[,2]))
                baddata <- is.na(subdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) 
                subdata <- subdata[!baddata,]
                subdata <-  subdata[order(subdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                          subdata$Hospital.Name),]    
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
        
        
}