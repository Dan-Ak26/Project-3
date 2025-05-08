#download the zip files from site
fileurl <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
#find the file on my desktop
destfile <- "C:/Users/Danie/Desktop/Project-3/rprog_data_ProgAssignment3-data.zip"
#download the files
download.file(fileurl, destfile)

install.packages(plyr)

library(plyr)
#set directory to working directory
dir <- "C;/Users/Danie/Desktop/Project-3/rprog_data_ProgAssignment3-data.zip"
#unzip the files
unzip(zipfile = "C:/Users/Danie/Desktop/Project-3/rprog_data_ProgAssignment3-data.zip", 
      exdir = "./Project-3", overwrite = TRUE)            
#read the files of outcome-of-care-measures.csv
outcome <- read.csv("Project-3/outcome-of-care-measures.csv", colClasses = "character")
#prints out top columns of outcome
head(outcome)

library("data.table")
library(dplyr)
#returns the number of columns
ncol(outcome)
#returns the number of rows
nrow(outcome)
#returns the names of outcome data frame
names(outcome)

#created a histogram for Heart Attack
outcome[, 11] <- as.numeric(outcome [, 11])
hist(outcome [, 11], xlab = "Deaths",
main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", col = "Red")


#histogram plot for heart failure
outcome[, 17] <- as.numeric(outcome [, 17])
hist(outcome [, 17], xlab = "Deaths",
main = "Hospital 30-Day Death (Mortality) Rates from Heart Failure", col = "Blue")


#histogram plot for heart failure
outcome[, 23] <- as.numeric(outcome [, 23])
hist(outcome [, 23], xlab = "Deaths",
main = "Hospital 30-Day Death (Mortality) Rates from pneumonia", col = "Purple")

best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("Project-3/outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  
 
  #I set outcome to health related problem an changed it if I wanted
  #to find hospital name
  outcome <-"heart attack"
 
  # I set state equal to different state every time 
  #to find different hospital names
  state <- "TX"
 
  #checks if state is in the data frame outcomes if not 
  #prints out a statement
  if(!state %in% outcomes$State){
    stop(" invalid state ")
  }
  
  valid_outcomes<- c("heart attack", "heart failure", "pneumonia")
  
 
  #checks if dataframe has the same row names
  if(!outcome %in% valid_outcomes){
    stop("invalid outcome") 
  
  }
    
  # checks exact columns and numbers you need
  outcome_cols <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  #filters out data by state 
  col_index <- switch(outcome, "heart attack" = 11,
                    "heart failure" = 17
                    , "pneumonia" =23)
  
  #Will create a smaller data frame with only43 the hospitals from that state.
  state_data <- outcomes[(outcomes$State == state), ]  
  
  #I converted the data frame into a numeric and suppressed the warning because I 
  #fix the issue in the code below.
  state_data[, col_index] <- suppressWarnings(as.numeric(state_data[, col_index]))
    
  #drops every row where the chosen column is an NA value.
  state_data <- state_data[ !is.na(state_data[, col_index]) , ]
  #order by outcome rates
  state_data <- state_data [order(state_data[, col_index ]) , ] 
                        
  
  #finds lowest value for state_data
  min_rate <- min(state_data[, col_index])
  
  #will return true or false based on value of min_rate 
  is_best <- state_data[, col_index] == min_rate
  #This will change by data frame to get me the hospital that is, is_best
  state_data2 <- state_data[ is_best, ]
  ## Return hospital name in that state with lowest 30-day death
  
  #This will give me the Hospital name of the state and the health problem
  Hospital_name <- state_data2[,"Hospital.Name"]
  best_sort <- sort(Hospital_name)
  best_sort[1]
  
  
  
  
}

#I called each of these to find 
#different hospital names based on
#the state and health related issues.
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "pnuemonia")
best("MD", "heart attack")



