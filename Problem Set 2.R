#1. 


voteTotals <- c(1245,925,1622,2101,42) #test data

benford <- function(x, leemis, choGains){ 
  #The following two variables will both be used as such for both Leemis and Cho Gain. They only need to be calculated once and can be used no matter which 'if' statement the function enters 
  sigDigitTable <- table(as.numeric(substr(x,1,1))) #takes the first number in each element and counts how many times each unique digit appears
  numTotals <- length(x) #counts how many elements in the vector. This is necessary for the lemis equation
  if(identical(leemis,TRUE) & identical(choGains,TRUE)){ #If the user inputs 'TRUE' for both Leemis and Cho Gains 
    m <- max((sigDigitTable/numTotals) - log10(1 + 1/as.numeric(labels(sigDigitTable)[[1]]))) #crux of the Leemis equation, follows the formula.  
    d <- sqrt(sum(((sigDigitTable/numTotals) - log10(1 + 1/as.numeric(labels(sigDigitTable)[[1]])))^2)) #crux of the leemis equation 
    return(list("Lemmis' m statistic" = m, "Cho Gains' d" = d, "Significant Digit Distribution" = sigDigit)) #Returns both values as a properly labeled list 
  }
  if(identical(leemis,TRUE)){ #If the user only sets Leemis as true
    m <- max((sigDigitTable/numTotals) - log10(1 + 1/as.numeric(labels(sigDigitTable)[[1]])))   #leemis function again
    return(list("Leemis' m statistic" = m, "Significant Digit Distribution" = sigDigit)) #returns as a properly labeled list
  }
  if(identical(choGains, TRUE)){ #if the user only sets Cho Gain as tue
    d <- sqrt(sum(((sigDigitTable/numTotals) - log10(1 + 1/as.numeric(labels(sigDigitTable)[[1]])))^2)) #Cho Gain again
    return(list("Cho Gains' d" = d, "Significant Digit Distribution" = sigDigit)) #returns as a properly labeled list
  }
  else{
    return("You must pick either/or/both Leemis and Cho Gains to be TRUE")
  }
}

benford(voteTotals,TRUE,FALSE) #Test of function. It works!

#2

asterisks <- function(input, leemisOrChoGains){ #subordinate function to add asterisks to leemis and Cho Gains values. To assess via leemis, set leemisOrChoGains to TRUE. For Cho Gains, set leemisOrChoGains to FALSE. 
  if(identical(leemisOrChoGains, TRUE)){
    if(input >= 1.212){
      return(paste(as.character(input),"***", sep=""))
    }
    if(input >= 0.967){
      return(paste(as.character(input),"**", sep=""))
    }
    if(input >= 0.851){
      return(paste(as.character(input),"*", sep=""))
    }
    if(input < 0.851){
      return(as.character(input))
    }
    if(is.na(input)){
      return(NA)
    }
  }
  if(identical(leemisOrChoGains, FALSE)){
    if(input >= 1.569){
      return(paste(as.character(input),"***", sep=""))
    }
    if(input >= 1.330){
      return(paste(as.character(input),"**", sep=""))
    }
    if(input >= 1.212){
      return(paste(as.character(input),"*", sep=""))
    }
    if(input < 1.212){
      return(as.character(input))
    }
    if(is.na(input)){
      return(NA)
    }
  }
}

install.packages("pander") #installs pander package to help create tables
print.benfords <- function(leemis, choGains){
  library("pander", lib.loc="~/R/win-library/3.4") #activates the stargazer package 
  leemisValue <- asterisks(leemis, TRUE) #TRUE for leemis significance
  choGainsValue <- asterisks(choGains, FALSE)  #FALSE for choGains significance
  matrix <- matrix(c(leemisValue,choGainsValue), ncol = 1) #create matrix of asterisk-ed values
  table <- data.frame(matrix) #sticks matrix in a dataframe
  rownames(table) <- c("Leemis' m", "Cho-Gains' d") #name rows 
  colnames(table) <- "Calculated Value" #name columns
  table <- pandoc.table(table, style = "grid", caption = "* = Significant at 0.10, ** = Significant at 0.05, *** = Significant at 0.01", plain.ascii= TRUE) #pander command to create table
  return(table) #return pander table
  detach("package:pander", unload=TRUE) #makes the package no longer usefull
}

createCSV <- function(leemis, choGains){ #function to create csv
  sink(file = "C:/Users/aaron/OneDrive/Documents/temp.csv", append = TRUE, split = FALSE) #creates file to write to
  input <- print.benfords(leemis, choGains) #set output to write
  sink(input) #write output to file
}

createCSV(0.8, 1.3) #test function

