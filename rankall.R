rankall <- function(outcome, num = "best") {
        ##Read outcome data
        outdata <- read.csv("outcome-of-care-measures.csv",
                            colClasses = "character")
        
        ##Check that outcome is valid
        if (!is.element(outcome, c("heart attack", "heart failure",
                                   "pneumonia"))) stop("invalid outcome")
        
        source("rankhospital.R")
        
        
        states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", 
                    "HI", "ID", "IL", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", 
                    "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY", "GU")
        
        hospitals <- NULL
        
        for (state in states) {
                name <- rankhospital(state, outcome, num)
                hospitals <- c(hospitals, name)
        }
        
        ranking <- data.frame(hospitals, states)
        colnames(ranking) <- c("hospital", "state")
        
        ranking
        
}