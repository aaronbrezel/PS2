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



print.benfords <- function(leemis, choGains){
  leemis <- asterisks(leemis)
  choGain <- asterisks(choGains)
  
}
paste(as.character(1352),"***", sep="")
