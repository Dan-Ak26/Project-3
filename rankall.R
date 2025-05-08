
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  read_data <- read.csv("Project-3/outcome-of-care-measures.csv", 
                       colClasses ="character",
                       header = TRUE)
  # I changed the outcome a couple of times to pneumonia and heart attack.
  outcome <- "heart failure"
  # I set state to the data frame State
  state <- read_data$State
  # outcomes and their corresponding column numbers
  valid_outcome3 <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23 )
  ## Check that state and outcome are valid
  if(all(!state %in% read_data$State)){
    stop("invalid state")
  }
  # Checks if outcome is valid
  if(!(outcome %in% names(valid_outcome3))){
    stop("invalid outcome")
  }
  # Get the column index
  outcol <- (valid_outcome3[[outcome]]) 
  #Wiil create a smaller data frame with onlys the hospitals from that state
  read_data2 <- read_data[(read_data$State == state) , ]
  #I converted the data frame into a numeric and suppressed the warning because I 
  #fix the issue in the code below.
  read_data2[, outcol] <- suppressWarnings(as.numeric(read_data2[[outcol]]))
  #drops every row where the chosen column is an NA value.
  read_data2 <- read_data2[!is.na(read_data2[, outcol]) , ]
  #Sort by the outcome column,picks alphabetically, and pick the hospital by num.
  state_groups <- split(read_data2, read_data2$State)
  
  
  #apply the same ranking logic to each state's hospital data.
  results <- lapply(state_groups, function(df){
    
   # df <- state_groups[[1]]
    num = "best"
    outcol <- (valid_outcome3[[outcome]])
    # Return hospital name in that state with the given rank.
    df <- df[order(df[ , outcol], df$Hospital.Name) , ]
    head(df)
    # if num is best then outputs. 
    if(num == "best"){
      num <- 1
     # if num is worst then it will return number of rows in data.frame 
    }else if(num == "worst"){
      num <-nrow(df)
    }
    num <- as.numeric(num)
   # if num is greater than number of rows it returns NA to Hospital.Name
    if(num > nrow(df)){
      hospital <- NA
    } else {
      hospital <- df[num, "Hospital.Name"]
      
    }
    #returns the 1st state in data frame.
    state <- df$State[1]
    #returns a data frame hospital, and state.
    data.frame(hospital = hospital, state = 
    state)
    
    
    
    
  })
  #Final cleanup, returns a data frame with the hospital names and 
  #state name
  final_df <- do.call(rbind, results)
  rownames(final_df) <- NULL
  final_df
  
}  
#returns the head or tail of each rankall
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure","worst"),10)
tail(rankall("heart failure","best"),10)
