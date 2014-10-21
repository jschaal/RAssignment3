source("rankhospital.R")
performTests <- function(x=vector()) {
    allTests <- length(x) == 0
    if (allTests | 1 %in% x) {
        print("Test 1")    
        print(rankhospital("TX", "heart failure", 4))
        writeLines("")
    }   
    
    if (allTests | 2 %in% x) {
        print("Test 2")    
        print(rankhospital("MD", "heart attack", "worst"))
        writeLines("")
    }
    
    if (allTests | 3 %in% x) {
        print("Test 3")    
        print(rankhospital("MN", "heart attack", 5000))
        writeLines("")
    }
    
    if (allTests | 4 %in% x) {
        print("Test 4")    
        print(rankhospital("NC", "heart attack", "worst"))
        writeLines("")
    }
    
    if (allTests | 5 %in% x) {
        print("Test 5")    
        print(rankhospital("WA", "heart attack", 7))
        writeLines("")
    }

    if (allTests | 6 %in% x) {
        print("Test 6")    
        print(rankhospital("WA", "pneumonia", 1000))
        writeLines("")
    }

    if (allTests | 7 %in% x) {
        print("Test 7")    
        print(rankhospital("NY", "heart attak", 7))
    }              
    
}