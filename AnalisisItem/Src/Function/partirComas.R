################################################################################
# # corItem.R
# # R Versions: Version 2.12.0
# #
# # Author(s): Jorge Mario Carrasco Ortiz
# #
# # Process: SABER 5° y 9° citizenship competencies
# # Description: Function to split a string which contains between words
# #
# # Inputs: a character string
# #
# # Outputs: a character vector with each word
# #
# # File history:
# #   20130624: Creation
################################################################################
partirComas <- function(x, car){
  # # Split strings with special characters
  # # Arg:
  # # x: String to split, example "1, 2, 3"
  # # car: The special character separating the string. Example: "," , "-"
  # #
  # # Ret:
  # # x: a Vector with c(1, 2, 3)
  kComas  <- length(gregexpr(car, x)[[1]])
  if(gregexpr(car, x)[[1]][1] == -1)  kComas <- 0
  expRegu <- paste(c("(.*)+", rep(paste(car, "(.*)+", sep = ""), kComas)), 
                   collapse = "")
  parGrPrin  <- regexec(expRegu, x)
  x <- regmatches(x, parGrPrin)[[1]][-1]
  return(x)
}



