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
        data[,11] <- as.numeric(data[,11])
        data <-  data[order(data$Hospital.Name),]
        if (outcome == "heart attack")
        {
                heartsubset <- data[,c(2,11)]
                bestHospital <- subset[which.min()]
                
             #   mindeaths <- min(data[,11], na.rm = T)
             #   > minhospitals <- data([,11]) == mindeaths      

        }

        outData
       ## colvals <- data[,c(1,outcome)]
}