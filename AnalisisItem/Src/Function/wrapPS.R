################################################################################
# # wrapPS.R
# # R Versions: 2.15.0
# #
# # Author(s): Víctor H. Cervantes - Álvaro Uzaheta
# #
# # Process: Interaction with Parscale from R
# # Description: Functions for reading Parscale output files
# #              and for running some basic setups of data
# #              from R using Parscale
# #
# # Inputs: None
 #
# # Outputs: Functions
# #
# # File history:
# #   20111128: Creation
# #   20121213: Added function to read the item information file (AU)
# #   20121213: Adjustments to style
# #   20121213: Parametrization of estimation method in RunParscale
# #             function
# #   20130503: Added parameters in RunParscale to fix item calibration
# #             and to print comments in the output files
################################################################################


################################################################################
# # Function for reading Parscale output file
################################################################################
ReadPsParFile <- function (fileName, filePath = "./") {
  # # Reads the item parameters from Parscale
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #
  # # Ret:
  # #  iPar: The item parameters as a data.frame or a list with elements
  # #    iPar with the item parameters and
  # #    iRater with rater severities
  # #    if a model with rater severities was estimated

  inFile <- file.path(filePath, fileName)
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  inFile <- file(inFile, "r")

  ################################################################################
  # # Read Parscale's file description lines
  ################################################################################
  comments        <- readLines(inFile, n = 2)
  testDescription <- read.fwf(inFile, c(8, rep(5, 5)), n = 1, colClasses = c("character", rep("numeric", 5)))
  names(testDescription) <- c("testName", "nBlocks", "nItemsTot", "modCode", "codDIFRaters", "nExtra")

  nItemsTot   <- as.numeric(testDescription["nItemsTot"])
  nBlocks     <- as.numeric(testDescription["nBlocks"])
  modCode     <- as.numeric(testDescription["modCode"])
  nItemsBlock <- as.numeric(read.fwf(inFile, rep(5, nItemsTot), n = 1))

  if (testDescription["codDIFRaters"] == 1) {
    nGroups <- as.numeric(testDescription["nExtra"])
  } else {
    nGroups <- 1
  }

  iPar <- data.frame(testName         = as.character(rep(testDescription["testName"], nItemsTot * nGroups)),
                     groupName        = character(length = nItemsTot * nGroups),
                     nameItem         = character(length = nItemsTot * nGroups),
                     blockName        = character(length = nItemsTot * nGroups),
                     discrimination   = numeric(length = nItemsTot * nGroups),
                     location         = numeric(length = nItemsTot * nGroups),
                     guessing         = numeric(length = nItemsTot * nGroups),
                     seDiscrimination = numeric(length = nItemsTot * nGroups),
                     seLocation       = numeric(length = nItemsTot * nGroups),
                     seGuessing       = numeric(length = nItemsTot * nGroups),
                     step.1           = numeric(length = nItemsTot * nGroups),
                     seStep.1         = numeric(length = nItemsTot * nGroups),
                     stringsAsFactors = FALSE)

  itemsRead <- 0
  for (ii in 1:nGroups) {
    ################################################################################
    # # Iterate through groups (for DIF)
    ################################################################################
    nameGroup <- readLines(inFile, n = 1)

    for (jj in 1:nBlocks) {
      ################################################################################
      # # Iterate though blocks
      ################################################################################
      itemRange <- itemsRead + 1:nItemsBlock[jj]

      # # Read specific item parameters
      iParBlock<- read.fwf(inFile, c(8, 5, 4, rep(10, 6)), n = nItemsBlock[jj],
                           colClasses = c("character", "numeric", "character", rep("numeric", 6)),
                           col.names = c("blockName", "nCats", "nameItem", "discrimination", "seDiscrimination", "location",
                                         "seLocation", "guessing", "seGuessing"))

      # # Read block common step parameters
      nCat <- unique(iParBlock[, "nCats"])
      commonSteps <- read.fwf(inFile, rep(10, nCat), n = 2)
      if (modCode <= 4) {
        commonSteps <- commonSteps[, -ncol(commonSteps)]
      } else {
        commonSteps <- commonSteps[, -1]
      }
      names(commonSteps) <- paste("step", 1:(nCat - 1), sep = "")

      # # Assign specific item parameters to the iPar data.frame
      iPar[itemRange, "groupName"] <- nameGroup
      iParNames <- which(names(iPar) %in% names(iParBlock))
      iParNames <- names(iPar)[iParNames]
      for (kk in iParNames) {
        iPar[itemRange, iParNames] <- iParBlock[, iParNames]
      }

      # # Assign common block step parameters to the iPar data.frame
      if (nCat > 2) {
        stepsInTable  <- grep("step", names(iPar), value = TRUE, ignore.case = TRUE)
        nStepsInTable <- length(stepsInTable) %/% 2
        nStepsMissing <- nCat - nStepsInTable - 1

        if (nStepsMissing > 0) {
          maxStepInTable <- nCat - nStepsMissing
          missingSteps   <- matrix(0, nrow = nrow(iPar), ncol = nStepsMissing * 2)
          colnames(missingSteps) <- paste(c("step", "seStep"), rep(maxStepInTable:(nCat - 1), each = 2), sep = ".")

          iPar <- data.frame(iPar, missingSteps)
          stepsInTable  <- grep("step", names(iPar), value = TRUE, ignore.case = TRUE)
        }

        stepsColumns   <- grep("se", stepsInTable, invert = TRUE, value = TRUE)
        seStepsColumns <- grep("se", stepsInTable, value = TRUE)
        iPar[itemRange, stepsColumns]   <- commonSteps[1, ]
        iPar[itemRange, seStepsColumns] <- commonSteps[2, ]
      }

      itemsRead <- itemsRead + nItemsBlock[jj]
    }  # for nBlocks
  }  # for nGroups

  if (testDescription["codDIFRaters"] == 2) {
    nRaters <- as.numeric(testDescription["nExtra"])
  }  else {
    nRaters <- 0
  }

  if (nRaters > 0) {
    iRaters <- read.fwf(inFile, c(8, 10, 10), colClasses = c("character", "numeric", "numeric"),
                        col.names = c("rater", "severity", "seSeverity"))
    iPar <- list(iPar = iPar, iRater = iRaters)
  }
  close(inFile)

  return(iPar)
}

ReadPsScoFile <- function (fileName, filePath = "./") {
  # # This function reads person ability estimates from Parscale
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #
  # # Ret:
  # #  pPar: The person abilities table as a data.frame

  inFile <- file.path(filePath, fileName)
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  system(paste("vim.exe \"+%s/\\(GROUP\\s\\d*.*\\)\\n/\\1/\" \"+x\"",
               inFile))
  pPar <- laf_open_fwf(filename = inFile,
                       column_widths = c(21, 1, 2, 7, 2,
                                         8, 2, 8, 1, 3, 2,
                                         8, 2, 3, 2, 3, 7,
                                         4, 7, 2, 7, 4,
                                         10, 2, 10),
                       column_types = rep("character", 25))

  pPar        <- pPar[, c(1, 4, 6, 8, 12, 14, 23, 25)]
  names(pPar) <- c("idSubject", "iSubject", "groupName",
                   "weight", "estMehtod", "idSubGroup",
                   "ability", "seAbility")
  close(file(inFile, "wt"))
  file.append(inFile, gsub(".PSLSC", ".PSLSC~", inFile))
  file.remove(gsub(".PSLSC", ".PSLSC~", inFile))

  for (pp in c("weight", "ability", "seAbility")) {
    pPar[, pp] <- as.numeric(pPar[, pp])
  }

  isEstimated <- pPar[, "ability"] != 999
  pPar[!isEstimated, "ability"] <- NA

  return(pPar)
}

ReadPsInfoFile <- function (fileName, filePath = "./") {
  # # This function reads the item information file from Parscale
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #
  # # Ret:
  # #  iInfo: The item information table as a data.frame

  inFile <- file.path(filePath, fileName)
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  iInfo <- read.fwf(inFile,
                    widths = c(-8, -2, 4, -2, 4, -2, -8, -2, 4, -2, 4,
                               -2, 2, -2, 10, -2, 18),
                    col.names = c("group", 'iBlock', "iItem", "item",
                                  "nQuadPoint", "measureQuadPoint",
                                  "information"),
                    colClasses = c("numeric", "numeric", "numeric",
                                   "character", "numeric", "numeric",
                                   "numeric"),
                    skip = 2)
  return(iInfo)
}


ReadPsFitFile <- function (fileName, filePath = "./") {
  # # This function reads the Parscale item fit contingency tables
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #
  # # Ret:
  # #  iFit

  inFile <- file.path(filePath, fileName)
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  inFile <- file(inFile, "r")
  iLine  <- 0

  ################################################################################
  # # Read Parscale's file description lines
  ################################################################################
  comments        <- readLines(inFile, n = 2)
  testDescription <- read.fwf(inFile, rep(4, 2), n = 1,
                              colClasses = rep("numeric", 2))
  names(testDescription) <- c("nSubtests", "nGroups")

  nSubtests   <- as.numeric(testDescription["nSubtests"])
  nGroups     <- as.numeric(testDescription["nGroups"])

  iLine <- iLine + 3


  iFit <- list()
  while (isOpen(inFile)) {

    readLine <- readLines(inFile, n = 1)
    iLine    <- iLine + 1


    ################################################################################
    # # Iterate through lines
    ################################################################################
    if (length(readLine) == 0) {
      close(inFile)
      break
    } else if (nchar(readLine) == 12) {
      isItem    <- FALSE
      isSubtest <- FALSE
    } else if (nchar(readLine) == 24) {
      isItem    <- TRUE
      isSubtest <- FALSE
    } else if (nchar(readLine) == 16) {
      isItem    <- FALSE
      isSubtest <- TRUE
    } else {
      errorMsg <- "Error determining type of following lines (group and bounds description or item contingency tables in line"
      stop(paste(errorMsg, iLine))
    }

    if (!isItem) {
      if (isSubtest) {
        ################################################################################
        # # Read subtest data
        ################################################################################
        iSubtest    <- as.numeric(substr(readLine, start = 1, stop = 4))
        nameSubtest <- substr(readLine, start = 5, stop = 12)
        nChiGroups  <- as.numeric(substr(readLine, start = 13, stop = 16))
        iLine       <- iLine + 1

      } else {

        iGroup    <- as.numeric(substr(readLine, start = 1, stop = 4))
        nameGroup <- substr(readLine, start = 5, stop = 12)

        if (nChiGroups > 8) {
          nGroupLines   <- nChiGroups %/% 8
          nExcessGroups <- nChiGroups %%  8
          widthsGroups  <- list()

          for (iiGroup in seq(nGroupLines)) {
            widthsGroups[[iiGroup]] <- rep(10, 8)
          }
          widthsGroups[[nGroupLines + 1]] <- rep(10, nExcessGroups)

          widthsGroups[seq(length.out = length(widthsGroups),
                           by = 2)] <- widthsGroups
          widthsGroups[seq(from = 2,
                           length.out = nGroupLines,  by = 2)] <- 0

          readGroups                      <- length(widthsGroups)

        } else {
          widthsGroups <- rep(10, nChiGroups)
          readGroups   <- 1
        }

        levelsChi <- c(as.matrix(read.fwf(inFile, widthsGroups, n = 1,
                              colClasses = "numeric")))
        levelsChi <- levelsChi[!is.na(levelsChi)]

        iLine     <- iLine + readGroups
      }
    }  else {

      iBlock <- as.numeric(substr(readLine, start = 1, stop = 4))
      nameBlock <- substr(readLine, start = 5, stop = 12)
      nCategories <- as.numeric(substr(readLine, start = 13, stop = 16))
      iItem  <- as.numeric(substr(readLine, start = 17, stop = 20))
      nameItem <- substr(readLine, start = 21, stop = 24)

      nPerGroup <- c(as.matrix(read.fwf(inFile, widthsGroups, n = 1,
                            colClasses = "numeric")))
      nPerGroup <- nPerGroup[!is.na(nPerGroup)]

      iLine     <- iLine + readGroups

      contingencyTable <- as.data.frame(matrix(nChiGroups * nCategories,
                                               nrow = nChiGroups,
                                               ncol = nCategories))
      expectedTable    <- as.data.frame(matrix(nChiGroups * nCategories,
                                               nrow = nChiGroups,
                                               ncol = nCategories))
      for (iiCategory in seq(nCategories)) {
        catName <- as.numeric(readLines(inFile, n = 1))
        iLine <- iLine + 1

        observed <- c(as.matrix(read.fwf(inFile,
                                         widthsGroups,
                                         n = 1,
                                         colClasses = "numeric")))
        observed <- observed[!is.na(observed)]
        contingencyTable[, iiCategory] <- observed

        expected <- c(as.matrix(read.fwf(inFile,
                                         widthsGroups,
                                         n = 1,
                                         colClasses = "numeric")))
        expected <- expected[!is.na(expected)]
        expectedTable[, iiCategory] <- expected

        iLine <- iLine + readGroups + readGroups

        if (is.numeric(catName)) {
          names(contingencyTable)[iiCategory] <- paste('cat', catName,
                                                       sep = '')
          names(expectedTable)[iiCategory] <- paste('cat', catName,
                                                    sep = '')
        } else {
          names(contingencyTable)[iiCategory] <- catName
          names(expectedTable)[iiCategory]    <- catName
        }


      }

      contingencyTable <- data.frame('iSubtest' = rep(iSubtest,
                                                      nChiGroups),
                                     'nameSubtest' = rep(nameSubtest,
                                                         nChiGroups),
                                     'iGroup' = rep(iGroup, nChiGroups),
                                     'nameGroup' = rep(nameGroup,
                                                       nChiGroups),
                                     'iBlock' = rep(iBlock, nChiGroups),
                                     'nameBlock' = rep(nameBlock,
                                                       nChiGroups),
                                     'iItem' = rep(iItem,
                                                   nChiGroups),
                                     'nameItem' = rep(nameItem,
                                                      nChiGroups),
                                     'thetaValues' = levelsChi,
                                     'nPerGroup' = nPerGroup,
                                     contingencyTable)

      expectedTable <- data.frame('iSubtest' = rep(iSubtest,
                                                   nChiGroups),
                                  'nameSubtest' = rep(nameSubtest,
                                                      nChiGroups),
                                  'iGroup' = rep(iGroup, nChiGroups),
                                  'nameGroup' = rep(nameGroup,
                                                    nChiGroups),
                                  'iBlock' = rep(iBlock, nChiGroups),
                                  'nameBlock' = rep(nameBlock,
                                                    nChiGroups),
                                  'iItem' = rep(iItem,
                                                nChiGroups),
                                  'nameItem' = rep(nameItem,
                                                   nChiGroups),
                                     'thetaValues' = levelsChi,
                                     'nPerGroup' = nPerGroup,
                                  expectedTable)


      itemTables <- list('observed' = contingencyTable,
                         'expected' = expectedTable)

      iFit[[iItem]] <- itemTables
      names(iFit)[iItem] <- nameItem

    }

  }

  return(iFit)
}

ReadPsChiFitFile <- function (fileName, filePath = "./") {
  # # This function reads the Parscale item fit tables from the PH2 output file
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #
  # # Ret:
  # #  iFit: the item fit table as a data.frame

  inFile <- file.path(filePath, fileName)
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filepath, "\n")
  }

  inFile <- file(inFile, "r")

  ################################################################################
  # # Search the start of the item fit information in the ph2 file
  ################################################################################
  fitPattern <- "ITEM FIT STATISTICS"
  isMatchLine <- FALSE

  readLines(file.path(filePath, fileName))

  while (!isMatchLine) {
    ph2Line <- readLines(inFile, n = 1)
    isMatchLine <- grepl(fitPattern, ph2Line)
  }

  ################################################################################
  # # Get the item fit information
  ################################################################################
  # # Initial values
  isDoneFit        <- FALSE
  multGroupPattern <- "TOTAL ITEM FIT STATISTICS"
  endFilePattern   <- "BYTES OF NUMERICAL"
  iFitPattern     <- "\\| (\\w+)? +\\| (\\w+) \\| +(\\d+\\.\\d+) +\\| +(\\d+)\\. +\\| +(0\\.\\d+)"
  iFitPatternAst     <- "\\| (\\w+)? +\\| (\\w+) \\| (\\S+) \\| +(\\d+)\\. +\\| +(0\\.\\d+)"
  groupNamePattern <- "GROUP NAME: (\\w+ ?\\w*)"

  groupName <- NULL
  blockName <- NULL
  nameItem  <- NULL
  chiValue  <- NULL
  dfChiVal  <- NULL
  pValueChi <- NULL

  # # Get the info
  while (!isDoneFit) {
    ph2Line <- readLines(inFile, n = 1)

    isMatchGroup <- grepl(groupNamePattern, ph2Line)
    if (isMatchGroup) {
      newGroup  <- unlist(regmatches(ph2Line,
                    regexec(groupNamePattern, ph2Line)))[2]
    }

    isWithAst <- grepl(iFitPatternAst, ph2Line)
    if (!isWithAst) {
      isMatch <- grepl(iFitPattern, ph2Line)
      matches <- unlist(regmatches(ph2Line, regexec(iFitPattern, ph2Line)))
    } else {
      isMatch <- grepl(iFitPatternAst, ph2Line)
      matches <- unlist(regmatches(ph2Line, regexec(iFitPatternAst, ph2Line)))
    }

    if (isMatch) {
      if (exists("newGroup")) {
        groupName <- c(groupName, newGroup)
      }

      if ("**********" %in% matches) {
        matches <- gsub("\\| (\\W+) +\\|", "| 100000.55 |", matches)
        matches <- gsub("^\\*+", "100000.55", matches)
      }

      if (nchar(matches[2]) > 0) {
        newBlock <- matches[2]
      }
      blockName <- c(blockName, newBlock)
      nameItem  <- c(nameItem, matches[3])
      chiValue  <- c(chiValue, as.numeric(matches[4]))
      dfChiVal  <- c(dfChiVal, as.numeric(matches[5]))
      pValueChi <- c(pValueChi, as.numeric(matches[6]))
    }

    isDoneFit <-  grepl(multGroupPattern, ph2Line) |  grepl(endFilePattern, ph2Line)
  }
  close(inFile)

  if (is.null(groupName)) {
    groupName <- rep("sample", length(blockName))
  }

  iFit <- data.frame(nameItem  = nameItem,
                     blockName = blockName,
                     groupName = groupName,
                     chiSquare = chiValue,
                     df        = dfChiVal,
                     pValue    = pValueChi)

  return(iFit)
}


################################################################################
# # Function to generate the Parscale control file and running the program
################################################################################
RunParscale <- function (responseMatrix, runName,
                         outPath = "../output", runPath = "../output",
                         group = NULL,
                         weights = NULL, personIds = NULL,
                         itemIds = NULL, rasch = FALSE, guessing = FALSE,
                         grm = TRUE, nQuadPoints = 30,
                         score = TRUE, personEstimation = "EAP",
                         thetaPriori = "GH", rsm = NULL, dif = NULL,
                         commonItems = NULL, logistic = TRUE, kD = NULL,
                         kChiGroups = 15,
                         srcPath = "../src/",
                         binPath = "../bin/", verbose = TRUE,
                         calibFile = NULL, skipVector = FALSE,
                         commentFile = NULL) {

  # # This function generates a Parscale control file given the options in its
  # # arguments, runs it and reads the item parameters
  # #
  # # Arg:
  # #  responseMatrix: numeric matrix or data.frame containing the (coded) responses to the items
  # #  runName: name of the stem used for the files related with the analyses
  # #  outPath: path where the files will be stored
  # #  group: vector with group numbers or single character codes of group membership
  # #  weights: vector with the weights to be asigned to each responses register
  # #  personIds: vector with the ids to be written for each responses register
  # #  itemIds: vector with the ids for each item
  # #  rasch: logical vector indicating if the models of each group ought to be Rasch-like (with all slopes contrained to be equal) or not
  # #  guessing: logical vector indicating if the models of each group
  # #            ought to have an estimate of guessing.
  # #  grm: logical indicating if the models ought to be estimated as Graded response model or Partial Credit model
  # #  nQuadPoints: number of quadrature points to be used for estimation
  # #  score: logical indicating whether or not person abilities should be obtained and stored
  # #  personEstimation: string scalar with the name of the estimation
  # #                    method to use for ability estimates (EAP, WML or MLE)
  # #  thetaPriori: string scalar with the name of the distribution to
  # #               use for ability prior distribution for EAP estimation
  # #  rsm: vector indicating the groups of items with the same rating scale model
  # #  dif: vector indicating the group of parameters to be estimated jointly (0) or separated (1)
  # #       (isSeparateSlopes, isSeparateThresholds, isSeparateCategories, isSeparateGuessing)
  # #  commonItems: vector indicating the position of items to be
  # #               considered common when running DIF model. Ignored if
  # #               dif argument is NULL

  # #  logistic: logical indicating whether or not using the logistic or the normal ogive models
  # #  kD: numeric constant to use as D for scaling parameters. Dafaults
  # #      to 1.702 if lofistic is TRUE and 1.0 if it is FALSE
  # #  kChiGroups: numbre of groups for chi-square item fit calculations
  # #  srcPath: path to working directory
    # #  binPath: path where the Parscale executables may be found
  # #  verbose: logical indicating whether to show or not function progres
  # #  calibFile: file name with the Parameter Calibration
  # #  skipVector: logical vector indicating if PARSCALE should estimate
  # #              items parameters or PARSCALE should use values
  # #              supplied by the user.
  # #  commentFile: Title with information of Test
  # #
  # # Ret:
  # #  iPar: returns the estimated item parameters

  ################################################################################
  # # Check arguments consistency
  ################################################################################
  if (is.null(calibFile) & any(skipVector)) {
      stop("calibFile should be defined")
  }
  if (!is.null(group)) {
    if (length(group) != nrow(responseMatrix)) {
      stop("group length and responses number of rows should be equal")
    }
  }
  if (!is.null(weights)) {
    if (length(weights) != nrow(responseMatrix)) {
      stop("weights length and responses number of rows should be equal")
    }
  }
  if (!is.null(personIds)) {
    if (length(personIds) != nrow(responseMatrix)) {
      stop("personIds length and responses number of rows should be equal")
    }
  }
  if (!is.null(itemIds)) {
    if (length(itemIds) != ncol(responseMatrix)) {
      stop("itemIds length and responses number of columns should be equal")
    }
    if (any(nchar(itemIds) > 7)) {
      stop("itemIds can only be four characters long")
    }
  }
  if (!is.null(rsm)) {
    if (length(rsm) != ncol(responseMatrix)) {
      stop("rsm length and responses number of columns should be equal")
    }
    if (length(rasch) != length(table(rsm))) {
      stop("rasch length ougth to be the same number of different blocks")
    }
    if (length(skipVector) != length(table(rsm))) {
      stop("skipVector length ougth to be the same number of different blocks")
    }
  } else {
    rsm <- seq(ncol(responseMatrix))
    if (length(rasch) == 1) {
      rasch <- rep(rasch, length(table(rsm)))
    }
    if (length(skipVector) == 1) {
       skipVector <- rep(skipVector, length(table(rsm)))
    }
    if (length(guessing) == 1) {
      guessing <- rep(guessing, length(table(rsm)))
    }
  }
  if (!is.null(dif)) {
    if (is.null(group)) {
      stop("Group must contain the group vector in order to run dif procedures\n")
    }
    if (!(any(dif %in% c(0, 1)))) {
      stop("All elements of if vector must be either 0 or 1\n")
    }
  } else {
    if (!is.null(group)) {
      if (verbose) {
        cat("dif not requested.\ngroup will not be used\n")
      }
    }
  }

  ################################################################################
  # # General variables and filenames
  ################################################################################
  # # Responses dimensions and category information
  responseMatrix   <- data.frame(responseMatrix)
  nItems           <- ncol(responseMatrix)
  nObservations    <- nrow(responseMatrix)
  nCategories      <- sapply(responseMatrix, function (x) length(table(x)))
  nCategoriesLen   <- unclass(by(nCategories, rsm, table))
  nCategoriesBlock <- sapply(nCategoriesLen, length)
  if (any(nCategoriesBlock) > 1) {
    stop("All items in the same block must have the same number of categories")
  }

  #  if (nItems > 9999) {
  #    stop("Cannot have more than 9999 items")
  #  }

  ################################################################################
  # # defined paths
  ################################################################################
  binPath <- file.path(srcPath, binPath)
  # #   outPath <- file.path(srcPath, outPath)
  runPath <- file.path(srcPath, runPath)
  ################################################################################
  # # Files used by Parscale
  ################################################################################
  # # Filenames
  dataFileName    <- paste(runName, ".dat", sep = "")
  omitkeyFileName <- paste(runName, ".omk", sep = "")
  notpresFileName <- paste(runName, ".npk", sep = "")

  itemFileName    <- file.path(outPath, paste(runName, ".pslip", sep = "")) #NO
  scoreFileName   <- file.path(outPath, paste(runName, ".pslsc", sep = "")) #NO
  infoFileName    <- file.path(outPath, paste(runName, ".pslii", sep = "")) #NO
  fitFileName     <- file.path(outPath, paste(runName, ".pslif", sep = "")) #NO
  # combineFileName <- paste(runName, ".pslcs", sep = "")

  # # With paths for the ones where data is written
  commandFile <- file.path(runPath, paste(runName, ".psl", sep = ""))
  dataFile    <- file.path(runPath, dataFileName)
  omitkeyFile <- file.path(runPath, omitkeyFileName)
  notpresFile <- file.path(runPath, notpresFileName)

  ################################################################################
  # # Generate data file
  ################################################################################
  #  if (nObservations > 999999) {
  #    stop("cannot have more than 999999 observations")
  #  }
  if (!is.null(weights)) {
    weights <- sprintf("%13.6f", weights)
  }

  ################################################################################
  # # Format identifiers for data matrix for Parscale
  ################################################################################
  # # Person identifiers
  if (!is.null(personIds)) {
    if (is.numeric(personIds)) {
      typeId <- "d" # # If numeric then add zeroes before the shorter ones
    } else {
      typeId <- "s" # # If not numeric then add spaces before the shortest ones
    }
  } else {
    personIds <- 1:nObservations
    typeId <- "d"
  }
  maxLengthId <- max(nchar(personIds))
  personIds   <- sprintf(paste("\"%0", maxLengthId, typeId, "\"", sep = ""), personIds)
  personIds   <- gsub("\"", "", personIds)

  # # Group identifiers
  if (!is.null(group)) {
    if (is.numeric(group)) {
      typeGroupId <- "d" # # If numeric then add zeroes before the shorter ones
    } else {
      typeGroupId <- "s" # # If not numeric then add spaces before the shortest ones
    }
  } else {
    group <- rep(1, nrow(responseMatrix))
    typeGroupId <- "d"
  }
  groupWidth <- max(nchar(group))
  group      <- sprintf(paste("\"%0", groupWidth, typeGroupId, "\"", sep = ""), group)
  group      <- gsub("\"", "", group)

  # # Item identifiers
  if (!is.null(itemIds)) {
    maxLengthIt <- max(nchar(itemIds))
    if (is.numeric(itemIds)) {
      typeId <- "d" # # If numeric then add zeroes before the shorter ones
    } else {
      typeId <- "s" # # If not numeric then add spaces before the shortest ones
    }
    itemIds   <- sprintf(paste("%0", maxLengthIt, typeId, sep = ""), itemIds)
  } else {
    itemIds     <- 1:nItems
    maxLengthIt <- max(nchar(itemIds))
    typeId      <- "d"
    itemIds     <- paste("it", sprintf(paste("\"%0", maxLengthIt, typeId, "\"", sep = ""), itemIds), sep = "")
    itemIds     <- gsub("\"", "", itemIds)
  }

  fillerColumn <- rep(" ", nrow(responseMatrix))
  if (!is.null(dif) & !is.null(weights)) {
    responseMatrix <- cbind(personIds, fillerColumn, group, fillerColumn, weights, fillerColumn, responseMatrix)
    omitLine       <- paste(rep("0", maxLengthId + 1 + groupWidth + 1 + 13 + 1 + nItems), collapse = "")
  } else if (!is.null(dif) & is.null(weights)) {
    responseMatrix <- cbind(personIds, fillerColumn, group, fillerColumn, responseMatrix)
    omitLine       <- paste(rep("0", maxLengthId + 1 + groupWidth + 1 + nItems), collapse = "")
  } else if (is.null(dif) & !is.null(weights)) {
    responseMatrix <- cbind(personIds, fillerColumn, weights, fillerColumn, responseMatrix)
    omitLine       <- paste(rep("0", maxLengthId + 1 + 13 + 1 + nItems), collapse = "")
  } else if (is.null(dif) & is.null(weights)) {
    responseMatrix <- cbind(personIds, fillerColumn, responseMatrix)
    omitLine       <- paste(rep("0", maxLengthId + 1 + nItems), collapse = "")
  }

  write.table(responseMatrix, file = dataFile, append = FALSE, sep = "", row.names = FALSE,
              col.names = FALSE, na = "0", quote = FALSE)
  write(omitLine, file = omitkeyFile)
  write(omitLine, file = notpresFile)


  # # TITLE
  cat("Running Parscale from R\n", commentFile, "\n", file = commandFile)


  # # COMMENT
  cat(">COMMENTS This model was run automatically from R\n", file = commandFile, append = TRUE)


  # # FILES
  cat(">FILES DFNAME = '", dataFileName, "', \n", sep = "", file = commandFile,
      append = TRUE)
  if (!is.null(calibFile)) {
    cat("       IFNAME = '", calibFile, "', \n", sep = "",
        file = commandFile, append = TRUE)
  }
  cat("       OFNAME = '", omitkeyFileName, "', \n", sep = "",
      file = commandFile, append = TRUE)
  cat("       NFNAME = '", notpresFileName, "', \n", sep = "",
      file = commandFile, append = TRUE)
  cat("       SAVE;\n", sep = "", file = commandFile, append = TRUE)


  # # SAVE
# #   cat(">SAVE PARM = '", toupper(gsub("/", "\\\\", itemFileName)),
# #       "', \n", sep = "", file = commandFile, append = TRUE)
# #   cat("      SCORE = '", toupper(gsub("/", "\\\\", scoreFileName)),
# #       "', \n", sep = "", file = commandFile, append = TRUE)
# #   cat("      INFORMATION = '", toupper(gsub("/", "\\\\", infoFileName)),
# #       "', \n", sep = "", file = commandFile, append = TRUE)
# #   cat("      FIT = '", toupper(gsub("/", "\\\\", fitFileName)),
# #       "';\n", sep = "", file = commandFile, append = TRUE)
# #
  cat(">SAVE PARM = '", toupper(itemFileName),
      "', \n", sep = "", file = commandFile, append = TRUE)
  cat("      SCORE = '", toupper(scoreFileName),
      "', \n", sep = "", file = commandFile, append = TRUE)
  cat("      INFORMATION = '", toupper(infoFileName),
      "', \n", sep = "", file = commandFile, append = TRUE)
  cat("      FIT = '", toupper(fitFileName),
      "';\n", sep = "", file = commandFile, append = TRUE)
  #  cat("      COMBINE = '", combineFileName, "';\n", sep = "", file = commandFile, append = TRUE)

  # # INPUT
  cat(">INPUT NTEST = 1, \n", sep = "", file = commandFile, append = TRUE)
  cat("       LENGTH = (", nItems, "), \n", sep = "", file = commandFile, append = TRUE)
  cat("       NFMT = 1, \n", sep = "", file = commandFile, append = TRUE)
  cat("       NIDCHAR = ", maxLengthId, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       NTOTAL = ", nItems, sep = "", file = commandFile, append = TRUE)
  if (!is.null(dif)) {
    nameGroups <- names(table(group))
    nGroups <- length(table(group))
    cat(",\n", file = commandFile, append = TRUE)
    cat("       MGROUP = ", nGroups, sep = "", file = commandFile, append = TRUE)
  }
  if (!is.null(weights)) {
    cat(",\n", file = commandFile, append = TRUE)
    cat("       WEIGHT\n", sep = "", file = commandFile, append = TRUE)
  }
  cat(";\n", file = commandFile, append = TRUE)

  # # (variable format statement)
  if (!is.null(dif) & !is.null(weights)) {
    cat("(", maxLengthId, "A1, 1X, ", groupWidth, "A1, 1X, 1F13.6, 1X, ", nItems, "A1)\n", sep = "", file = commandFile, append = TRUE)
  } else if (!is.null(dif) & is.null(weights)) {
    cat("(", maxLengthId, "A1, 1X, ", groupWidth, "A1, 1X, ", nItems, "A1)\n", sep = "", file = commandFile, append = TRUE)
  } else if (is.null(dif) & !is.null(weights)) {
    cat("(", maxLengthId, "A1, 1X, 1F13.6, 1X, ", nItems, "A1)\n", sep = "", file = commandFile, append = TRUE)
  } else if (is.null(dif) & is.null(weights)) {
    cat("(", maxLengthId, "A1, 1X, ", nItems, "A1)\n", sep = "", file = commandFile, append = TRUE)
  }

  # # TEST
  nBlocks <- length(table(rsm))

  cat(">TEST TNAME = RunR, \n", sep = "", file = commandFile, append = TRUE)
  cat("      NBLOCK = ", nBlocks, ",\n", sep = "", file = commandFile, append = TRUE)
  cat("      ITEMS = (1(1)", nItems, "), \n", sep = "", file = commandFile, append = TRUE)
  cat("      INAME = (", sep = "", file = commandFile, append = TRUE)
  for (ii in 1:nItems) {
    if (ii == 1) {
      spaces <- ""
    }else {
      spaces <- "               "
    }
    cat(spaces, itemIds[ii], sep = "", file = commandFile, append = TRUE)
    if (ii < nItems) {
      cat(",\n", file = commandFile, append = TRUE)
    } else {
      cat(");\n", file = commandFile, append = TRUE)
    }
  }

  # # BLOCK
  nItemsBlock <- table(rsm)
  for (ii in seq(nBlocks)) {
    cat(">BLOCK", ii, " BNAME = Block", ii, ", \n", sep = "", file = commandFile, append = TRUE)
    cat("         NITEMS = ", nItemsBlock[ii], ", \n", sep = "", file = commandFile, append = TRUE)
    cat("         NCAT = ", nCategories[ii], sep = "", file = commandFile, append = TRUE)
    if (rasch[ii]) {
      cat(", CSLOPE", sep = "", file = commandFile, append = TRUE)
    }
    if (skipVector[ii]) {
      cat(", SKIP ", sep = "", file = commandFile, append = TRUE)
    }


    cat(";\n", sep = "", file = commandFile, append = TRUE)
  }

  # # MGROUP
  if (!is.null(dif)) {
    cat(">MGROUP GCODE = ('", file = commandFile, append = TRUE)
    for (ii in seq(nGroups)) {
      cat(nameGroups[ii], sep = "", file = commandFile, append = TRUE)
      if (ii < nGroups) {
        cat("', '", file = commandFile, append = TRUE)
      } else {
        cat("'), ", file = commandFile, append = TRUE)
      }
    }

    if (!is.null(commonItems)) {
      cat("COMMON = ('", file = commandFile, append = TRUE)

    }
    
    # #       (isSeparateSlopes, isSeparateThresholds, isSeparateCategories, isSeparateGuessing)
    isSeparateSlopes     <- dif[1]
    isSeparateThresholds <- dif[2]
    isSeparateCategories <- dif[3]
    isSeparateGuessing   <- dif[4]

    if (rasch) {
      if (isSeparateSlopes == 0) {
        isSeparateSlopes <- 1 # # Strange Parscale behaviour
      }
    }
    cat("DIF = (", isSeparateSlopes, ", ", isSeparateThresholds, ", ",
        isSeparateCategories, ", ", isSeparateGuessing, ");\n",
        sep = "", file = commandFile, append = TRUE)
  }

  # # CALIB
  cat(">CALIB ", file = commandFile, append = TRUE)
  if (grm) {
    cat("GRADED, \n", file = commandFile, append = TRUE)
  } else {
    cat("PARTIAL, \n", file = commandFile, append = TRUE)
  }
  if (logistic) {
    if (is.null(kD)) {
      kD <- 1.702
    }
    cat("       LOGISTIC, \n", file = commandFile, append = TRUE)
  } else {
    if (is.null(kD)) {
      kD <- 1.0
    }
    cat("       NORMAL, \n", file = commandFile, append = TRUE)
  }
  cat("       SCALE = ", kD, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       ITEMFIT = ", kChiGroups, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       NQPT = ", nQuadPoints, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       POSTERIOR, \n", file = commandFile, append = TRUE)
  cat("       CYCLES = (25,2,2,2,2), \n", file = commandFile, append = TRUE)
  cat("       NEWTON = 5, \n", file = commandFile, append = TRUE)
  if (!is.null(dif)) {
  cat("       FREE = (0, 1, COMBINED, POSTERIOR), \n", file = commandFile, append = TRUE)

  }
  cat("       FLOAT;\n", file = commandFile, append = TRUE)

  # # SCORE
  cat(">SCORE NQPT = ", nQuadPoints, ", \n", sep = "", file = commandFile, append = TRUE)
  if (!(personEstimation %in% c("EAP", "WML", "MLE"))) {
    personEstimation <- "EAP"
  }
  cat("       ", personEstimation, sep = "", file = commandFile, append = TRUE)
  if (personEstimation == "EAP") {
    priorDistribution <- switch(thetaPriori,
                                uniform = 1, # Uniform distribution
                                normal  = 2, # Normal on equally spaced points
                                GH      = 3, # Normal on Gauss-Hermite points
                                2)
    cat(priorDistribution, sep = "", file = commandFile, append = TRUE)
  }
  if (!score) {
    cat(", NOSCORE", sep = "", file = commandFile, append = TRUE)
  }
  cat(";\n", sep = "", file = commandFile, append = TRUE)

  batDir <- file.path(runPath, "WinXP-RUNPARSCALE.bat")
# #   batDir <- file.path(outPath, "WinXP-RUNPARSCALE.bat")
  if (file.exists(batDir)) {
    file.remove(batDir)
  }

  cat(paste(gsub("/", "\\\\", file.path(binPath, "psl0.eje")),
            ' ', runName, "\n", sep = ""), file = batDir,
      append = TRUE)
  cat(paste(gsub("/", "\\\\", file.path(binPath, "psl1.eje")),
            " ", runName, "\n", sep = ""), file = batDir,
      append = TRUE)
  cat(paste(gsub("/", "\\\\", file.path(binPath, "psl2.eje")),
            " ", runName, "\n", sep = ""), file = batDir,
      append = TRUE)
  cat(paste(gsub("/", "\\\\", file.path(binPath, "psl3.eje")),
            " ", runName, "\n", sep = ""), file = batDir,
      append = TRUE)

  setwd(runPath)
# #   setwd(outPath)
  system("WinXP-RUNPARSCALE.bat")
  setwd(srcPath)

}


################################################################################
# # Rutina que revisa si el archivo PH2 tiene un error por estimabilidad
# # de los par'ametros de localizaci'on en PCM GRM 
################################################################################

testPH2FilePS  <- function(fileName, filePath = "./", nItems ){

      # # read PH2 file and ask about the problem with estimation
      # # process
      # #
      # # Arg:
      # #  fileName         : The file name
      # #  filePath         : The file path
      # #  nItems [numeric] : number of Items
      # #
      # # Ret:  data.frame with part of PH2 file where the proccess stops

      headPattStart  <- "PARSCALE V4.1"
      headPattFinish <- "CALIBRATION OF MAINTEST"
      errorPattern   <- c("ESTIMATION IS ABORTED", "MATRIX IS SINGULAR")
      filePH2        <- file.path(filePath, fileName)
      stringsPH2     <- readLines(filePH2)
      isAbortedPH2R   <- lapply(errorPattern, grepl, stringsPH2)
      isAbortedPH2RR  <- sapply(isAbortedPH2R, function(x)any(x))
      if(any(isAbortedPH2RR)){
           lineError      <- which(unlist(isAbortedPH2R[isAbortedPH2RR])) + 1
           toPrintError   <- stringsPH2[(lineError - (nItems * 3 + 5)):(lineError-1)]
           toPrintError

           headPH2Start   <- which(grepl(headPattStart, stringsPH2))
           headPH2Finish  <- which(grepl(headPattFinish, stringsPH2))
           toPrintHead    <- stringsPH2[headPH2Start:(headPH2Finish+3)]
           quitEmpty      <- grepl("^$", toPrintHead)
           toPrintHead    <- toPrintHead[!quitEmpty]
           toPrint        <- c(toPrintHead,toPrintError)
           writeLines(toPrint)
           toPrint        <- data.frame(toPrint)
           ret            <- list(error = TRUE, fragment = toPrint)
           return(ret)
           }else return(list(error = FALSE))
}







