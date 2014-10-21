source("best.R")
performTests <- function(testNumbers = vector()) {
    
    allTests <- length(testNumbers) ==0
    
    if (allTests || 1 %in% testNumbers)
    {
        print("Test 1")
        print(best("TX", "heart attack"))
        writeLines("")
    }
    
    if (allTests || 2 %in% testNumbers)
    {
        print("Test 2")
        print(best("TX", "heart failure"))
        writeLines("")
    }
    
    if (allTests || 3 %in% testNumbers)
    {
        
        print("Test 3")
        print(best("MD", "heart attack"))
        writeLines("")
    }
    
    if (allTests || 4 %in% testNumbers)
    {
        print("Test 4")
        print(best("MD", "pneumonia"))
        writeLines("")
    }
    ## errors
    if ( 5 %in% testNumbers)
    {
        print("Test 5")
        best("BB", "heart attack")
    }
    
    if ( 6 %in% testNumbers)
    {
        print("Test 6")
        best("NY", "hert attack")
    }
}