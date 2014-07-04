rankhospital <- function(state, outcome, num = "best") {
        ##Read outcome data
        outdata <- read.csv("outcome-of-care-measures.csv",
                            colClasses = "character")
        
        ##Check that state and outcome are valid
        if (!is.element(state, outdata[, 7])) stop("invalid state")
        if (!is.element(outcome, c("heart attack", "heart failure",
                                   "pneumonia"))) stop("invalid outcome")
        
        ## Return hospital name in that state with the given rank
        # set outcome variable value
        if (outcome == "heart attack") {
                outcomevalue <- 11
        } else if (outcome == "heart failure") {
                outcomevalue <- 17
        } else if (outcome == "pneumonia") {
                outcomevalue <- 23
        }
        
        hospitals <- NULL
        
        #subset state data
        s <- subset(outdata, outdata[, 7] == state)
        
        #remove NAs
        s <- subset(s, !is.na(s[, outcomevalue]))
        
        best <- min(s[, outcomevalue], na.rm = TRUE)
        
        s[, outcomevalue] <- suppressWarnings(as.numeric(s[, outcomevalue]))
        subset <- data.frame(s[, 2], s[, outcomevalue])
        
        colnames(subset) <- c("Hospital.Name", "Rate")
        
        scrubber <- complete.cases(subset)
        clean <- subset[scrubber, ]
        sort <- order(clean$Rate)
        finalsubset <- clean[sort, ]
        
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
                num <- nrow(finalsubset)
        }
        
        as.character(finalsubset[num, 1])
}