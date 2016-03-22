
# Reads outcome-of-care-measures.csv from working dir and returns
# hospital name with the lowest 30-day mortality for
# the specified outcome in that state.

# Valid values for outcome are: "heart attack", "heart failure", "pneumonia"

# In case of a tie, returns the name of hospital that is first after all contending
# hospital char vectors have been sorted alphabetically.


best <- function(state, outcome) {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    valid_states <- unique(df$State)
    if (!(state %in% valid_states)) {
        stop("invalid state")
    }
    ## if (!(outcome %in% valid_outcomes)) {
    ##    stop("invalid outcome")
    ## }
    
    if (outcome == "heart attack") {
        col_num <- 11
    } else if (outcome == "heart failure") {
        col_num <- 17
    } else if (outcome == "pneumonia") {
        col_num <- 23
    } else {
        stop("invalid outcome")
    }
    
    ## Filter outcome to only include values from state
    df.state <- df[which(df[, 7] == state), ]
    
    ## Get hospital name(s) with lowest 30-day death rate for outcome
    
    # Set dtype for selected column
    df.state[, col_num] <- as.double(df.state[, col_num]) 
    min_val <- min(df.state[, col_num], na.rm=TRUE) # convenience var
    df.min <- df.state[which(df.state[, col_num] == min_val), ] 
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if (length(df.min) > 1) {
        #return(sort(df.min[, 2])[1])
        return(df.min[, 2])
    } else {
        return(df.min[, 2])
    }
}