################################################################################
# # MeasurementModelsWithLavaan.R
# # R Versions: 2.14.0
# #
# # Author(s): Víctor H. Cervantes
# #
# # Process: Test structure analysis
# # Description: Functions to translate a certain factor structure into lavaan syntax, for running those models and for
# # comparing different nested measurement models
# #
# # Inputs: None
# #
# # Outputs: Functions
# #
# # File history:
# #   20111123: Creation
# #   20111205: Added generation of contraints to groups of equal loadings
# #
# # TODO: Add funtion that generates constraints from a set of equalities and a set of given values
################################################################################

################################################################################
# # Functions related with the factor structure of the measurement model
################################################################################
GenerateStructure <- function (measureModel) {
  # # Generates a factor structure for the wrapping function from a measureModel data.frame
  # #
  # # Arg:
  # #  measureModel: a data.frame containing columns:
  # #    indicator: the name of the indicator variables
  # #    factor: the name of the factor each indicator belongs to. Notice an indicator may appear in several rows of the
  # #            data frame if it is an indicator of several factors.
  # #
  # # Ret:
  # #  factorStructure
  
  if (!is.data.frame(measureModel)) {
    stop("measureModel must be a data.frame")
  }

  variables    <- c("indicator", "factor")
  isMeasureModel <- all(names(measureModel) %in% variables)
  if (!isMeasureModel) {
    stop("measureModel must contain an indicator variable and a factor variable")
  }

  factorStructure <- by(measureModel$indicator, measureModel$factor, function (x) as.character(x))
  factorStructure <- unclass(factorStructure)
  namesFactorStructure <- names(factorStructure)
  attributes(factorStructure) <- NULL
  names(factorStructure) <- namesFactorStructure
  
  return(factorStructure)
}

GetStructure <- function (loadings, cutOff = 0.25) {
  # # Generates a factor structure for the wrapping function from a loadings matrix
  # #
  # # Arg:
  # #  loadings: a matrix of factor loadings
  # #  cutOff: a value that indicates the least absolute value loading that will be considered to generate the factor
  # #     structure
  # #
  # # Ret:
  # #  factorStructure
  
  if (length(dim(loadings)) != 2) {
    stop("loadings must be a two-dimensional matrix or a data.frame")
  } 

  isCompIndicator <- data.frame(abs(loadings) > cutOff)
  factorStructure <- lapply(isCompIndicator, function (x) rownames(isCompIndicator)[seq(along = x)[x]])
  
  return(factorStructure)
}

GenerateFixedStructure <- function (measureModel) {
  # # Generates a factor structure for the wrapping function from a measureModel data.frame with the values to be fixed
  # #
  # # Arg:
  # #  measureModel: a data.frame containing columns:
  # #    indicator: the name of the indicator variables
  # #    factor: the name of the factor each indicator belongs to. Notice an indicator may appear in several rows of the
  # #            data frame if it is an indicator of several factors.
  # #    loading: value to fix in the data
  # #
  # # Ret:
  # #  factorStructure
  
  if (!is.data.frame(measureModel)) {
    stop("measureModel must be a data.frame")
  }

  variables    <- c("indicator", "factor", "loading")
  isMeasureModel <- all(names(measureModel) %in% variables)
  if (!isMeasureModel) {
    stop("measureModel must contain an indicator variable, a factor variable and a loading variable")
  }

  MakeFixedLoadings <- function (x) {
    fixedValues <- x[, 2]
    names(fixedValues) <- x[, 1]

    return(fixedValues)
  }
  factorStructure <- by(measureModel[, c("indicator", "loading")], measureModel[, "factor"], MakeFixedLoadings)
  factorStructure <- unclass(factorStructure)
  namesFactorStructure <- names(factorStructure)
  attributes(factorStructure) <- NULL
  names(factorStructure) <- namesFactorStructure
  
  return(factorStructure)
}

GenerateConstrainsStructure <- function (measureModel) {
  # # Generates a factor structure for the wrapping function from a measureModel data.frame with the values to be fixed
  # #
  # # Arg:
  # #  measureModel: a data.frame containing columns:
  # #    indicator: the name of the indicator variables
  # #    factor: the name of the factor each indicator belongs to. Notice an indicator may appear in several rows of the
  # #            data frame if it is an indicator of several factors.
  # #    loading: coding where indicators to be constrained equal have the same code
  # #
  # # Ret:
  # #  factorStructure
  
  if (!is.data.frame(measureModel)) {
    stop("measureModel must be a data.frame")
  }

  variables    <- c("indicator", "factor", "loading")
  isMeasureModel <- all(names(measureModel) %in% variables)
  if (!isMeasureModel) {
    stop("measureModel must contain an indicator variable, a factor variable and a loading variable")
  }

  MakeFixedLoadings <- function (x, factorName) {
    GetGroups <- function(x, factorName) {
      return(paste("equal(\"", factorName, " =~ ", as.character(x[1]), "\")", sep = ""))
    }
    fixedValues <- rep(by(x[, 1], x[, 2], GetGroups, factorName), length(x[, 2]))
    names(fixedValues) <- x[, 1]
    fixedValues <- fixedValues[-1]

    return(fixedValues)
  }

  factorStructure <- list()
  for (factorName in unique(measureModel[, "factor"])) {
    isFactor <- measureModel[, "factor"] == factorName
    factorStructure[[factorName]] <- MakeFixedLoadings(measureModel[isFactor, c("indicator", "loading")], factorName)
  }
  
  return(factorStructure)
}

################################################################################
# # Function for generating the lavaan model object
################################################################################
factorStructure2LavaanModel <- function (factorStructure, fixedLoadings = NULL) {
  # # Produces a model specification in lavaan format from the factorStructure list
  # #
  # # Arg:
  # #  factorStructure: a named list of factors containing the names of the indicator variables 
  # #  fixedLoadings: if specified, a named list with named numeric vectors indicating the values of the fixed loadings
  # #
  # # Ret:
  # #  modLavaan: a model specification for lavaan
  
  if (!is.list(factorStructure)) {
    stop("factorStructure must be a list")
  }

  tempFile <- "tempModel"
  cat("# # Measurement model\n", file = tempFile)
  for (ii in names(fixedLoadings)) {
    isFixValues <- which(factorStructure[[ii]] %in% names(fixedLoadings[[ii]]))
    for (jj in isFixValues) {
      factorStructure[[ii]][jj] <- paste(fixedLoadings[[ii]][factorStructure[[ii]][jj]],
                                         factorStructure[[ii]][jj], sep = " * ")
    }
  }

  for (ii in seq(along = factorStructure)) {
    cat(names(factorStructure)[ii], " =~ ", paste(factorStructure[[ii]], collapse = " + "), "\n", sep = "",
        file = tempFile, append = TRUE)
  }

  modLavaan <- readLines(tempFile)
  file.remove(tempFile)
  
  return(modLavaan)
}

