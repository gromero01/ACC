#################################################################################
# # wrapBL.R
# # R Versions: 2.15.0
# #
# # Author(s): Jorge Mario Carrasco Ortiz
# #
# # Process: Interaction with Bilog from R
# # Description: Functions for reading Bilog output files
# #              and for running some basic setups of data
# #              from R using Bilog
# #
# # Inputs: None
 #
# # Outputs: Functions
# #
# # File history:
# #   20151206: Creation
# #   20121207: Added function to read the item information file (AU)
################################################################################

################################################################################
# # Function for reading Parscale output file
################################################################################

ReadBlParFile <- function (fileName, filePath = "./") {
  # # Reads the item parameters from Bilog
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #
  # # Ret:
  # #    iPar: The item parameters as a data.frame or a list with elements
  # #    iPar with the item parameters and
  # #    iRater with rater severities
  # #    if a model with rater severities was estimated

  inFile <- file.path(filePath, fileName)

  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }
  inFile <- file(inFile, "r")

  ################################################################################
  # # Read Bilog's file (PAR file)
  ################################################################################
  parametros  <- read.fwf(inFile, skip = 4, header = FALSE,
                          widths = c(8, 10, 8, rep(10, 11), 16),
                          colClasses = c("character", "character",
                                        rep("numeric", 13)))

  colnames(parametros)   <- c("item", "prueba", "intercepto", "eeinter",
                              "disc", "eedisc", "dif", "eedif", "dispersion",
                              "eedisp", "azar", "eeazar", "drift", "eedrift",
                              "cons")
  parametros[, "item"]   <- gsub(' ', '', parametros[, 'item'])
  parametros[, "prueba"] <- gsub(' ', '', parametros[, 'prueba'])
  close(inFile)
  return(parametros)
}

ReadBlTCTFile <- function (fileName, filePath = "./") {
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

  ################################################################################
  # # Read Bilog's file (TCT file)
  ################################################################################
  auxHead        <- readLines(inFile)
  auxHead        <- which(nchar(gsub("-", "", auxHead)) == 1)
  names(auxHead) <- c("ItInicial", "ItFinal")

  tctData  <- read.fwf(inFile, skip = auxHead["ItInicial"], header = FALSE,
                       widths = c(5, 11, 11, 10, 8, 9, 10, 9),
                       n = auxHead["ItFinal"] - auxHead["ItInicial"] - 1,
                       colClasses = c("numeric", "character", rep("numeric", 6)))
  colnames(tctData) <- c("order", "item", "TRIED", "RIGHT", "PCT",
                            "LOGIT", "PEARSON", "BISERIAL")
  tctData[, "item"]    <- gsub("^\\s+|\\s+$", "", tctData[, "item"])

  return(tctData)
}

ReadBlScoFile <- function (fileName, filePath = "./", lengthIds = 8) {
  # # This function reads person ability estimates from Parscale
  # #
  # # Arg:
  # #  fileName:  The file name
  # #  filePath:  The file path
  # #  lengthIds: The length of the key
  # # Ret:
  # #  readFile: The person abilities table as a data.frame

  inFile <- file.path(filePath, fileName)
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }
  readFile <- readLines(inFile)
  indPegue <- data.frame(colId = seq(1, length(readFile), 2), 
                         colHab = seq(2, length(readFile), 2))
  readFile <- apply(indPegue, 1, function(x) paste(readFile[x], collapse = ""))[-1]
  readFile <- gsub("\\s+", " ", readFile)
  readFile <- read.delim(text = readFile, sep = " ", header = FALSE, 
                         colClasses = c("integer", rep("character", 2),
                           "double", "character", "integer",
                           "integer", rep("double", 5)))

  names(readFile) <- c("C1", "GROUP", "ID", "WEIGHT", "TEST", "TRIED",
                       "RIGHT", "PERCENT", "ABILITY", "SERROR",
                       "C11", "C12")
  readFile[, "iSubject"]<- 1:nrow(readFile)
  return(readFile[, !names(readFile) %in% c("C1", "C11", "C12")])
}


################################################################################
# # Function to generate the Parscale control file and running the program
################################################################################

RunFromWN <- function(runPath, scriptPath) {
   auxDir    <- getwd()
   batchFile <- file.path(runPath, "WinXP-RUNBILOG-PL.bat")
   cat('SET SCRIPT_PATH=', scriptPath, '\n', file = batchFile, sep = "")
   cat("::\n:: Genera archivos BILOG desde WINSTEPS\n::\n", file = batchFile, append = TRUE)
 
   cat("%SCRIPT_PATH%ParcharITEM1NAMELEN.pl\n", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%WS2BILJJ.pl -blm -dat -key -cortar\n\n", file = batchFile, append = TRUE)
 
   cat("::\n:: Corre prueba y anula items por correlacion menor que 0.05\n::", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%CorrerBLM.pl -pba -f1\n", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%BilogEliminarCorrelacion.pl -cor 0.05\n", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%WS2BILJJ.pl -blm -cortarGrado\n", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%CorrerBLM.pl -pba -f3\n", file = batchFile, append = TRUE)
 
   cat("%SCRIPT_PATH%BILOGcheckPH2.pl -t >revisionPH2.txt", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%BILOGsco2icc.pl -mid 24 -hab 2.0", file = batchFile, append = TRUE)
 
   cat(":: calcular Correlacion!", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%RunRCorrelacion.pl", file = batchFile, append = TRUE)
   cat(":: graficar", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%RunRGraficos.pl ", file = batchFile, append = TRUE)
   cat("call CrearGraficasICC.BAT %SCRIPT_PATH%", file = batchFile, append = TRUE)
 
   cat("%SCRIPT_PATH%BILOGgetItemSTATS.pl -t", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%BILOGgetSCO2.pl -t", file = batchFile, append = TRUE)
   cat("%SCRIPT_PATH%consolidarDatosReporte.pl", file = batchFile, append = TRUE)
 
   setwd(runPath)
   system("WinXP-RUNBILOG-PL.bat")
   setwd(auxDir)
}

RunBilog <- function (responseMatrix, runName, outPath = "./",
                      runPath = "./", weights = NULL, group = NULL,
                      personIds = NULL, itemIds = NULL, guessing = FALSE,
                      nQuadPoints = 30, score = TRUE, personEstimation = "EAP",
                      logistic = TRUE, kD = NULL, dif = NULL,
                      srcPath = "../src/", binPath = "../bin/", verbose = TRUE,
                      commentFile = NULL,  calibFile = NULL,
                      runProgram = TRUE, itNumber = NULL, NPArm = 2){

    # # This function generates a Parscale control file given the options in its
    # # arguments, runs it and reads the item parameters
    # #
    # # Arg:
    # #  responseMatrix: numeric matrix or data.frame containing the (coded) responses to the items
    # #  runName: name of the stem used for the files related with the analyses
    # #  outPath: path where the files will be stored
    # #  weights: vector with the weights to be asigned to each responses register
    # #  group: vector with group numbers or single character codes of group membership
    # #  personIds: vector with the ids to be written for each responses register
    # #  itemIds: vector with the ids for each item
    # #  nQuadPoints: number of quadrature points to be used for estimation
    # #  score: logical indicating whether or not person abilities should be obtained and stored
    # #  personEstimation: string scalar with the name of the estimation
    # #                    method to use for ability estimates (EAP, WML or MLE)
    # #  kD: numeric constant to use as D for scaling parameters. Dafaults
    # #      to 1.702 if lofistic is TRUE and 1.0 if it is FALSE
    # #  kChiGroups: numbre of groups for chi-square item fit calculations
    # #  dif: vector indicating the group of parameters to be estimated jointly (0) or separated (1)
    # #       (isSeparateSlopes, isSeparateThresholds, isSeparateCategories, isSeparateGuessing)
    # #  srcPath: path to working directory
    # #  binPath: path where the Parscale executables may be found
    # #  verbose: logical indicating whether to show or not function progres
    # #  commentFile: Title with information of Test
    # #  calibFile: file name with the Parameter Calibration
    # #  runProgram:
    # #  NPArm: Number of parameters of the model
    # # Ret:
    # #  iPar: returns the estimated item parameters
    ################################################################################
    # # Check arguments consistency
    ################################################################################

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
    if (any(nchar(itemIds) > 30)) {
      stop("itemIds can only be four characters long")
    }
  }

  ################################################################################
  # # General variables and filenames
  ################################################################################
  # # Responses dimensions and category information
  nItems           <- ncol(responseMatrix)
  nObservations    <- nrow(responseMatrix)

  # # defined paths
  binPath <- file.path(srcPath, binPath)
  runPath <- file.path(srcPath, runPath)


  # # Files used by Bilog
  dataFileName    <- paste(runName, ".dat.cal", sep = "")
  omitkeyFileName <- paste(runName, ".om", sep = "")
  keyFileName     <- paste(runName, ".key", sep = "")
  notpresFileName <- paste(runName, ".np", sep = "")

  itemFileName  <- file.path(outPath, paste(runName, ".PAR", sep = "")) #NO
  tctFileName   <- file.path(outPath, paste(runName, ".TCT", sep = "")) #NO
  scoreFileName <- file.path(outPath, paste(runName, ".SCO", sep = "")) #NO

  #infoFileName    <- file.path(outPath, paste(runName, ".pslii", sep = "")) #NO
  #fitFileName     <- file.path(outPath, paste(runName, ".pslif", sep = "")) #NO
  # combineFileName <- paste(runName, ".pslcs", sep = "")

  # # With paths for the ones where data is written
  commandFile <- file.path(runPath, paste(runName, ".blm", sep = ""))
  dataFile    <- file.path(runPath, dataFileName)
  keyFile     <- file.path(runPath, keyFileName)
  omitkeyFile <- file.path(runPath, omitkeyFileName)
  notpresFile <- file.path(runPath, notpresFileName)

  ################################################################################
  # # Generate data file
  ################################################################################
  if (!is.null(weights)) {
    weights <- sprintf("%13.6f", weights)
  }

  ################################################################################
  # # Format identifiers for data matrix for Bilog
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
  personIds   <- sprintf(paste0("\"%0", maxLengthId, typeId, "\""), personIds)
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
  group      <- sprintf(paste("\"%", groupWidth, typeGroupId, "\"", sep = ""), group)
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

  fillerColumn <- rep("", nrow(responseMatrix))
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
    keyLine        <- paste(c(rep(" ", maxLengthId), rep("1", nItems)), collapse = "")
    omitLine       <- paste(c(rep(" ", maxLengthId), rep("9", nItems)), collapse = "")
    notprLin       <- paste(rep(" ", maxLengthId + nItems), collapse = "")
  }

  write.table(responseMatrix, file = dataFile, append = FALSE, sep = "",
              row.names = FALSE, na = " ", col.names = FALSE, quote = FALSE)
  write(omitLine, file = omitkeyFile)
  write(notprLin, file = notpresFile)
  write(keyLine, file = keyFile)

  if (verbose) cat("Datafile prepared\n")

  # # TITLE
  cat("Running Bilog from R\nPrueba", commentFile, "\n", file = commandFile)

  # # COMMENT
  cat(">COMMENT\nThis model was run automatically from R\n\n", file = commandFile, append = TRUE)


  # # FILES
  cat(">GLOBAL DFNAME = '", dataFileName, "', \n", sep = "", file = commandFile,
      append = TRUE)
  if (!is.null(calibFile)) {
    cat("       IFNAME = '", calibFile, "', \n", sep = "",
        file = commandFile, append = TRUE)
  }
  cat("        NPArm = ", NPArm, ",\n", sep = "", file = commandFile, append = TRUE)
  cat("        NTEST = 1,\n", file = commandFile, append = TRUE)
  cat("        SAVE;\n", sep = "", file = commandFile, append = TRUE)

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
  cat("      ISTAT = '", toupper(tctFileName),
      "', \n", sep = "", file = commandFile, append = TRUE)
  cat("      SCORE = '", toupper(scoreFileName),
      "'; \n", sep = "", file = commandFile, append = TRUE)

  #  cat("      COMBINE = '", combineFileName, "';\n", sep = "", file = commandFile, append = TRUE)
  cat(">LENGTH NITems = (", nItems, "); \n", sep = "", file = commandFile, append = TRUE)
  # # INPUT
  cat(">INPUT NTOTAL = ", nItems, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       KFNAME = '", keyFileName, "', \n", sep = "", file = commandFile, append = TRUE)
  cat("       OFNAME = '", omitkeyFileName, "', \n", sep = "", file = commandFile, append = TRUE)
  cat("       NFNAME = '", notpresFileName, "', \n", sep = "", file = commandFile, append = TRUE)
  cat("       NALt = 1000, \n", file = commandFile, append = TRUE)
  cat("       NIDCHAR = ", maxLengthId, "; \n", sep = "", file = commandFile, append = TRUE)
  # if (!is.null(dif)) {
  #   nameGroups <- names(table(group))
  #   nGroups <- length(table(group))
  #   cat(",\n", file = commandFile, append = TRUE)
  #   cat("       MGROUP = ", nGroups, sep = "", file = commandFile, append = TRUE)
  # }
  # if (!is.null(weights)) {
  #   cat(",\n", file = commandFile, append = TRUE)
  #   cat("       WEIGHT\n", sep = "", file = commandFile, append = TRUE)
  # }
  # cat(";\n", file = commandFile, append = TRUE)

  # # ITEMS
  cat(">ITEMS INUMBERS = 1(1)", nItems, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       INAMES = (\n", file = commandFile, append = TRUE)
  auxItems <- strwrap(paste(itemIds, collapse = ", "), width = 80)
  cat(gsub("\\s", "", auxItems), sep = "\n", file = commandFile, append = TRUE)
  cat("                );\n", sep = "", file = commandFile, append = TRUE)

  # # TEST
  cat(">TEST \n      TNAME = '", substr(gsub("_V.+", "", runName), 1, 8), "', \n",
    sep = "", file = commandFile, append = TRUE)
  cat("      INUmber = (\n", file = commandFile, append = TRUE)
  if (is.null(itNumber)){
    auxItems <- strwrap(paste(seq(length(itemIds)), collapse = ", "),
                        width = 30)
    cat(gsub("\\s", "", auxItems), sep = "\n", file = commandFile, append = TRUE)
  } else {
    auxItems <- strwrap(paste(itNumber, collapse = ", "),  width = 30)
    cat(gsub("\\s", "", auxItems), sep = "\n", file = commandFile, append = TRUE)
  }
  cat("                );\n", sep = "", file = commandFile, append = TRUE)

  # # (variable format statement)
  if (!is.null(dif) & !is.null(weights)) {
    cat("(", maxLengthId, "A1, ", groupWidth, "A1, 1X, 1F13.6, 1X, ", nItems, " A1)\n",
        sep = "", file = commandFile, append = TRUE)
  } else if (!is.null(dif) & is.null(weights)) {
    cat("(", maxLengthId, "A1, ", groupWidth, "A1, 1X, ", nItems, " A1)\n", sep = "", file = commandFile, append = TRUE)
  } else if (is.null(dif) & !is.null(weights)) {
    cat("(", maxLengthId, "A1, 1F13.6, 1X, ", nItems, " A1)\n", sep = "", file = commandFile, append = TRUE)
  } else if (is.null(dif) & is.null(weights)) {
    cat("(", maxLengthId, "A1, ", nItems, " A1)\n", sep = "", file = commandFile, append = TRUE)
  }

  # # CALIB
  cat(">CALIB \n", file = commandFile, append = TRUE)
  cat("       CYCLES = 500, \n", file = commandFile, append = TRUE)
  cat("       NEWTON = 30, \n", file = commandFile, append = TRUE)
  cat("       NQPT = ", nQuadPoints, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       NOSprior, \n", file = commandFile, append = TRUE)
  cat("       DIAGNOSIS = 1, \n", file = commandFile, append = TRUE)
  cat("       CRIT = 0.0001 \n", file = commandFile, append = TRUE)
  cat("       ;\n", file = commandFile, append = TRUE)

  # # SCORE
  cat(">SCORE NQPT = ", nQuadPoints, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       RSCTYPE = 0 \n",  file = commandFile, append = TRUE)
  if (!score) {
    cat(", NOSCORE ", sep = "", file = commandFile, append = TRUE)
  }
  cat(";\n", sep = "", file = commandFile, append = TRUE)
  if (verbose) cat("Control file prepared\n")

  batDir <- file.path(runPath, "WinXP-RUNBILOG.bat")

  if (file.exists(batDir)) {
    file.remove(batDir)
  }

  cat(paste(gsub("/", "\\\\", file.path(binPath, "BLM1.eje")),
            " ", runName, " > ", paste0(runName, ".log1"),
            "\n", sep = ""), file = batDir)
  cat(paste(gsub("/", "\\\\", file.path(binPath, "BLM2.eje")),
            " ", runName, " > ", paste0(runName, ".log2"),
            "\n", sep = ""), file = batDir,
      append = TRUE)
  cat(paste(gsub("/", "\\\\", file.path(binPath, "BLM3.eje")),
            " ", runName, " > ", paste0(runName, ".log3"),
            "\n", sep = ""), file = batDir,
      append = TRUE)

  setwd(runPath)
  if (runProgram) {
    system("WinXP-RUNBILOG.bat")
  }
  setwd(srcPath)
}
