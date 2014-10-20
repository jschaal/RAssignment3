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
        if (outcome == "heart attack")
        {
                outData <- "heart attack"
        }

        outData
       ## colvals <- data[,c(1,outcome)]
}