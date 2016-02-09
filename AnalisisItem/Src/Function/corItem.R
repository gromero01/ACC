################################################################################
# # corItem.R
# # R Versions: Version 2.12.0
# #
# # Author(s): Ronald Herrera & Alvaro Uzaheta
# #
# # Process: SABER 5° y 9° citizenship competencies
# # Description: Function to compute the correlation between the item and the
# #             index that the Item belong
# #
# # Inputs: a data.frame
# #
# # Outputs: a data.frame with the correlation
# #
# # File history:
# #   20111124: Creation
################################################################################

CorItem <- function (dataFrame) {
  # # description
  # #
  # # Arg:
  # #  dataFrame: [data.frame] data.frame with the items to compute the correlation 
  # #       between the total of items and the item, the total excluded the item.
  # #       This function accepts binary items or factor variables.     
  # #
  # # Ret:
  # #  [data.frame] the name of the item and the correlation
  
  # # check is the df has only factor variables or numeric
  nCols <- ncol(dataFrame)
  isNumericas <- all(sapply(dataFrame, is.numeric))
  if (!isNumericas) {
    base <- sapply(dataFrame, as.numeric)
  } else {
    base <- dataFrame
  }

  base <- sapply(dataFrame, as.numeric)

  # # compute the correlation allow missing values in the data frame
  totalWoI <- rowSums(base, na.rm = TRUE) - base
  correlation <- diag(cor(base, totalWoI, use = "pairwise.complete.obs"))
  return(data.frame(item = names(correlation), correlation = correlation))
}
