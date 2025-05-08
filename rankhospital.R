
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("Project-3/outcome-of-care-measures.csv",
                        colClasses = "character", header = TRUE)
  #I set num equal to best or worst depending if I wanted to change it 
  num = "best"
  #I set outcome to heart failure or other health related issues if I wanted to change it.
  outcome <- "heart failure"
  #I changed the state depending on the result I wanted.
  state <- "TX"
  # outcomes and their corresponding column numbers
  valid_outcome2 <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" =23)
  ## Check that state and outcome are valid
  if(!state %in% data$State){
    stop(" invalid state ")
  }
  
  if(!(outcome %in% names(valid_outcome2))){
    stop("invalid outcome")
  }
  # Get the column index
  out_col <- valid_outcome2[[ outcome ]]
  
  #Wiil create a smaller data frame with onlys the hospitals from that state
  data2_ <- data[(data$State == state) , ]
  #I converted the data frame into a numeric and suppressed the warning because I 
  #fix the issue in the code below.
  data2_[, out_col] <- suppressWarnings(as.numeric(data2_[, out_col]))
  #Sort by the outcome column,picks alphabetically, and pick the hospital by num.
  data2_ <- data2_[!is.na(data2_[, out_col]) , ]
  
  
  # if num is best then outputs. 
  if(num == "best"){
    num <- 1
  }
  # if num is worst then it will return number of rows in data.frame
  if(num == "worst"){
    num<- nrow(data2_)
  }
  
  ## Return hospital name in that state with the given rank
  data2_<-data2_[order(data2_[[ out_col]], data2_[["Hospital.Name"]])  , ]
  # to find the first 6 columns of hospitals
  head(data2_,6)
  
  #will give me the hospital name based on if hospital is best or worst.
  data2_$Hospital.Name[num]
  ## 30-day death rate
}
# Will return the hospital that I want.
rankhospital("TX","heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("TX", "heart failure", "best")
