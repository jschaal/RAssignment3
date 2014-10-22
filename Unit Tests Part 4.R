source("rankall.R")
print(head(rankall("heart attack", 20), 10))
print(tail(rankall("pneumonia", "worst"), 3))
print(tail(rankall("heart failure"), 10))
