#1. 

voteTotals <- c(1245,925,1622,2101,42) #test data

leemis <- function(x){ 
  sigDigitTable <- table(as.numeric(substr(x,1,1))) #takes the first number in each element and counts how many times each unique digit appears
  numTotals <- length(x) #counts how many elements in the vector. This is necessary for the lemis equation
  m <- max((sigDigitTable/numTotals) - log10(1 + 1/as.numeric(labels(sigDigitTable)[[1]]))) #crux of the lemis equation, follows the formula.  
  return(m)
}

leemis(voteTotals) #Test of function. It works!
