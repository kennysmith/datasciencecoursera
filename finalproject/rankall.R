
## Returns a dataframe containing the hospitals with the specified rank (num)
## in terms of 30-day mortality rate for every state.

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that outcome is valid and assign colnum if so
    if (outcome == "heart attack") {
        col_num <- 11
    } else if (outcome == "heart failure") {
        col_num <- 17
    } else if (outcome == "pneumonia") {
        col_num <- 23
    } else {
        stop("invalid outcome")
    }
    
    ## Check that num is valid
    if (!(num == "best" | num == "worst" | is.numeric(num))) {
        stop("invalid num")
    }
    
    ## Get list of unique states
    valid_states <- sort(unique(df$State))
    
    # Create container for hospital names and state abbreviations
    hospitals <- character()
    states <- character()
    for (i in 1:length(valid_states)) {
        df.state <- df[which(df[, 7] == valid_states[i]), ]
        
        # Set dtype for selected column
        df.state[, col_num] <- as.double(df.state[, col_num]) 
        df.state <- df.state[, c(2, 7, col_num)]
        
        df.state <- df.state[complete.cases(df.state),] # Remove na
        
        ## Sort column values by hospital name and rate
        df.state <- df.state[order(df.state[,3], df.state[,1]), ]
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if (num == "best") {
            n <- 1
        } else if (num == "worst") {
            n <- nrow(df.state)
        } else {
            n <- num
        }
        hosp <- df.state[n, 1]
        hospitals[i] <- hosp
        states[i] <- df.state[n, 2]
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return(data.frame(hospitals, states))
}