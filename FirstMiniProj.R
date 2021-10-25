## Aira Aim Seterra
## CMSC 197-First Mini Project


##____________________________________________________________________________(1)


## Setting up the directory
setwd("C:/Users/Aim/Desktop")


## Creating a Function "pollutantMean" 
pollutantMean <- function(directory, pollutant, id= 1:332){
  
  ## Listing the files in the directory
  directory_files <- dir(path = directory, pattern = ".csv", full.names = TRUE) 
  
  ## Creating empty data frame with numeric objects
  merge_files <- numeric()
  
  ## For loop through the list of files until id is found
  for (i in id){
    ##Reading the .csv files
    read_data <-read.csv(directory_files[i], header= TRUE)
    ##Adding files to the main data frame "merge_files"
    merge_files<-c(merge_files,read_data[[pollutant]])
  } 
  ## Returns the mean of pollutants and ignoring NA values
  mean(merge_files, na.rm = TRUE)
}


## Sample code to run to check the outputs

pollutantMean("specdata", "sulfate", 69:96)
##[1] 3.20281

pollutantMean("specdata", "nitrate", 100:105)
##[1] 1.697431


##____________________________________________________________________________(2)


setwd("C:/Users/Aim/Desktop")


## Creating a Function "complete"
complete <- function(directory, id= 1:332){
  
  ## Listing the files in the directory
  directory_files <- dir(path = directory, pattern = ".csv", full.names = TRUE) 
  
  ## Creating empty data frame with numeric objects
  nobs <- numeric()
  
  ## For loop through the list of files until id is found
  for (i in id){
    
    ## Reading the .csv files
    read_data <-read.csv(directory_files[i], header= TRUE)
    ## Computing the sum of complete cases
    nobs <- c(nobs, sum(complete.cases(read_data)))

  } 
  data.frame(id, nobs)
}


## Sample code to run to check the outputs

complete("specdata", 23)
##   id nobs
##1  23 492

complete("specdata", c(3, 12, 21, 30, 33, 42, 51,60))
##   id nobs
##1  3  243
##2 12   96
##3 21  426
##4 30  932
##5 33  466
##6 42   60
##7 51  193
##8 60  448



##____________________________________________________________________________(3)


setwd("C:/Users/Aim/Desktop")

## Creating a Function "corr"
corr <- function(directory, threshold=0){
  
  ## Listing the files in the directory
  directory_files <- dir(path = directory, pattern = ".csv", full.names = TRUE) 
  
  ## Creating an empty vector
  correlations <- c()
  
  ## For loop through the list of files until id is found
  for (i in 1:332){
    
    ## Reading the .csv files
    read_data <-read.csv(directory_files[i], header= TRUE)
    
    ## Storing completely observed cases read from read_data in a vector "obs" 
    obs <- read_data[complete.cases(read_data),]   
   
    ## Using if else control structure
     if(nrow(obs) > threshold){  
       ## If the number of completely observed cases (on all variables) is greater than the threshold,
       ## it calculates the correlation between sulfate and nitrate for monitor locations and it will update 
       ## every iteration of the vector "correlations"
      correlations <- c(correlations,cor(obs$sulfate,obs$nitrate))
       }                                          
    else 0
    ## or, else
  }
  ## it will  return a numeric vector of length 0
  return(correlations)
}

## Sample code to run to check the outputs
cr <- corr("specdata", 46)
head(cr); summary(cr); length(cr)
##[1] -0.22255256 -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667
##      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##    -0.46127 -0.06061  0.08955  0.11432  0.26106  0.78596 
##[1] 299


##____________________________________________________________________________(4)


## Setting up the directory
setwd("C:/Users/Aim/Desktop/HospData")

## Reads the outcome data into R via the read.csv function
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
## Run the code below to view the first few rows
head(outcome)

## Run the code below to make a simple histogram of the 30-day death rates from heart attach
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

## Modify above code so that you can plot the 30-day mortality rates for heart attack
## given the dataset outcome-of-care-measures.csv.

## Creating a vector named "mortality" and 
## using as.numeric function to coerce the column to be numeric
mortality <- as.numeric(outcome[, 11]) 

## Creating a Histogram based on the example output
hist(mortality, main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
     xlab = "Deaths", ylab = "Frequency", col = "Light Blue")


