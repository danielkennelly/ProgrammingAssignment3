best <- function(state, outcome) {
        ##Read outcome data
        outdata <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character")
        
        ##Check that state and outcome are valid
        if (!is.element(state, outdata[, 7])) stop("invalid state")
        if (!is.element(outcome, c("heart attack", "heart failure",
                                  "pneumonia"))) stop("invalid outcome")
        
        ##Return hospital name in that state with lowest 30-day death rate
        # set outcome variable value
        if (outcome == "heart attack") {
                outcomevalue <- 11
        } else if (outcome == "heart failure") {
                outcomevalue <- 17
        } else if (outcome == "pneumonia") {
                outcomevalue <- 23
        }
        
        #loop through data, collect hospital names with best value
        hospitals <- NULL
        
        #subset state data
        s <- subset(outdata, outdata[, 7] == state)
        #remove NAs
        s <- subset(s, !is.na(s[, outcomevalue]))
        
        best <- min(s[, outcomevalue], na.rm = TRUE)
        
        s[, outcomevalue] <- suppressWarnings(as.numeric(s[, outcomevalue]))
        subset <- data.frame(s[, 2], s[, outcomevalue])
        
        colnames(subset) <- c("name", "rate")
        
        scrubber <- complete.cases(subset)
        clean <- subset[scrubber, ]
        sort <- order(clean$rate)
        finalsubset <- clean[sort, ]
        bestvalue <- finalsubset[1, 2]
        
        hospitals <- subset(finalsubset, finalsubset$rate == bestvalue)
        if (nrow(hospitals) > 1) {
                sort <- order(hospitals$name)
                sorted <- hospitals[sort, ]
                as.character(sorted[1,1])
        } else {
                as.character(hospitals[1,1])
        }
}