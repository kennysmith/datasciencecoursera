rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state is valid
    valid_states <- unique(df$State)
    if (!(state %in% valid_states)) {
        stop("invalid state")
    }
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
    
    ## Filter outcome to only include values from state
    df.state <- df[which(df[, 7] == state), ]
    
    # Set dtype for selected column
    df.state[, col_num] <- as.double(df.state[, col_num]) 
    df.state <- df.state[, c(2, 7, col_num)]
    
    df.state <- df.state[complete.cases(df.state),] # Remove na
    
    ## Sort column values by hospital name and rate
    df.state <- df.state[order(df.state[,3], df.state[,1]), ]
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    return(df.state[num,1])
}