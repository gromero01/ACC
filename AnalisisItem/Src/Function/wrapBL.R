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

readDIFF <- function(fileName, filePath = "./"){
  # # Reads the DIFF output from Bilog
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #
  # # Ret:
  # #
  inFile   <- file.path(filePath, fileName)
  datDIFF  <- readLines(inFile)
  idInicio <- grep("^\\s+$", datDIFF) 
  idFin    <- c(idInicio[2], length(datDIFF))
  arreglarFilas <- function(datDIFF, x) {
    auxDat <- datDIFF[idInicio[x]:idFin[x]]
    auxDat <- auxDat[-grep("^\\s+$", auxDat)]
    auxDat <- paste0(auxDat[seq(1, length(auxDat), by = 2)], auxDat[seq(2, length(auxDat), by = 2)])
    auxDat <- str_split_fixed(auxDat, pattern = "\\s+", n = ifelse(x == 2, 4, 6))
    data.frame(auxDat, stringsAsFactors = FALSE)
  }
  datDIFF <- sapply(1:2, function(x) arreglarFilas(datDIFF, x))
  datDIFF <- merge(datDIFF[[1]], datDIFF[[2]], by = c("X1", "X2"))
  names(datDIFF) <- c("PBA", "ITEM", "DIFICULTAD_BASE", "DIFICULTAD_NUEVA", 
                      "se_DIFICULTAD_BASE", "se_DIFICULTAD_NUEVA", "DIFF", "se_DIFF")
  datDIFF[, -(1:2)] <- sapply(datDIFF[, -(1:2)], as.numeric)
  datDIFF <- subset(datDIFF, !((DIFICULTAD_BASE == 0 & se_DIFICULTAD_BASE == 0) | (DIFICULTAD_NUEVA == 0 & se_DIFICULTAD_NUEVA == 0)))
  datDIFF[,"Z"] <- datDIFF[,"DIFF"] / datDIFF[,"se_DIFF"]
  datDIFF[, "pvalue"] <- round(pnorm(datDIFF[,"Z"]), 4)
  return(datDIFF)
}


 readPH2CHI <- function(fileName, filePath = "./"){
  # # Reads the item parameters from Bilog
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #
  # # Ret:
  # #
  require(dplyr)
  inFile <- file.path(filePath, fileName)
  data   <- readLines(inFile)
  linea  <- grep(pattern = "QUADRATURE POINTS, POSTERIOR WEIGHTS", data)
  indPri <- grep("ITEM PARAMETERS AFTER CYCLE", data)
  data  <- data[(max(indPri) + 5):(linea - 17)]
  sp    <- seq(from = 3, to = (3 * (length(data) + 1) / 3 - 2), by = 3) 
  tab <- gsub("\\n", "", data[-sp] %>% paste(collapse = "\n"))
  tab <- data.table('Original' = strsplit(tab, ") I")[[1]])
  tab <- tab[, data.table::tstrsplit(Original, "|", fixed=TRUE)]
  tab <- tab[, list('item'  =  gsub("I", "", V1), 
                    'p_val_chi2' = as.numeric(gsub("\\(|\\)", "", V13)), 
                    'chi2'       = as.numeric(gsub("(\\d.+)\\s+(\\d.+)", "\\1", V7)), 
                    'gl_chi2'    = as.numeric(gsub("(\\d.+)\\s+(\\d.+)", "\\2", V7)))]
  tab <- tab[, item := paste0("I", gsub("\\s", "", item))]
  tab <- subset(tab, item != "I")
  return(tab)
}



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
  auxHead        <- auxHead[(length(auxHead) - 1):length(auxHead)]
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
  inFile   <- gsub("\\.SCO", ".SCO.red", inFile)
  cat(readFile, sep = '\n', file = inFile)
 
  laf      <- laf_open_csv(filename = inFile, sep = " ", trim = TRUE,
                           column_types = c("integer", rep("string", 2),
                           "double", "string", "integer",
                           "integer", rep("double", 5)), 
                           column_names = c("C1", "GROUP", "ID", 
                                            "WEIGHT", "TEST", "TRIED",
                                            "RIGHT", "PERCENT", 
                                            "ABILITY", "SERROR",
                                            "C11", "C12"))
  readFile <- laf[, ]
  # readFile <- read.delim(text = readFile, sep = " ", header = FALSE, 
  #                        colClasses = c("integer", rep("character", 2),
  #                          "double", "character", "integer",
  #                          "integer", rep("double", 5)))

  # names(readFile) <- c("C1", "GROUP", "ID", "WEIGHT", "TEST", "TRIED",
  #                      "RIGHT", "PERCENT", "ABILITY", "SERROR",
  #                      "C11", "C12")
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
                      logistic = TRUE, kD = NULL, dif = FALSE,
                      srcPath = "../src/", binPath = "../bin/", verbose = TRUE,
                      commentFile = NULL,  calibFile = NULL,
                      runProgram = TRUE, itNumber = NULL, NPArm = 2, 
                      thrCorr = 0.05, datAnclas = NULL, flagSPrior = FALSE, 
                      flagTPrior = FALSE, flagRASCH = FALSE){
    
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
    # #  dif: logical indicating the DIFF procces
    # #  srcPath: path to working directory
    # #  binPath: path where the Parscale executables may be found
    # #  verbose: logical indicating whether to show or not function progres
    # #  commentFile: Title with information of Test
    # #  calibFile: file name with the Parameter Calibration
    # #  runProgram:
    # #  NPArm: Number of parameters of the model
    # #  datAnclas: Data Frame with items parameters to estimate
    # #  flagSPrior:
    # #  flagTPrior:
    # #  flagRASCH: Perform RASCH 1PL Model
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
  prFileName      <- paste(runName, ".prm", sep = "")
  omitkeyFileName <- paste(runName, ".om", sep = "")
  keyFileName     <- paste(runName, ".key", sep = "")
  notpresFileName <- paste(runName, ".np", sep = "")

  itemFileName  <- file.path(outPath, paste(runName, ".PAR", sep = "")) #NO
  tctFileName   <- file.path(outPath, paste(runName, ".TCT", sep = "")) #NO
  scoreFileName <- file.path(outPath, paste(runName, ".SCO", sep = "")) #NO
  diffFileName  <- file.path(outPath, paste(runName, ".DIF", sep = "")) #NO

  #infoFileName    <- file.path(outPath, paste(runName, ".pslii", sep = "")) #NO
  #fitFileName     <- file.path(outPath, paste(runName, ".pslif", sep = "")) #NO
  # combineFileName <- paste(runName, ".pslcs", sep = "")

  # # With paths for the ones where data is written
  commandFile <- file.path(runPath, paste(runName, ".blm", sep = ""))
  dataFile    <- file.path(runPath, dataFileName)
  prFile      <- file.path(runPath, prFileName)
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
    if (!is.numeric(group)) {
      group      <- as.factor(group)
      groupLabel <- levels(group)
      group      <- as.numeric(group)
      typeGroupId <- "d"
    }
  } else {
    group       <- rep(1, nrow(responseMatrix))
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


  # # Delete all missing
  isBad <- rowMeans(is.na(responseMatrix)) == 1
  if (any(isBad)) {
    responseMatrix <- responseMatrix[!isBad, ]
  }

  # # Fix dat.cal 
  fillerColumn <- rep("", nrow(responseMatrix))
  if (dif & !is.null(weights)) {
    responseMatrix <- cbind(personIds, fillerColumn, group, fillerColumn, weights, fillerColumn, responseMatrix)
    auxLength      <- maxLengthId + groupWidth + max(nchar(weights))   
  } else if (dif & is.null(weights)) {
    responseMatrix <- cbind(personIds, fillerColumn, group, fillerColumn, responseMatrix)
    auxLength      <- maxLengthId + groupWidth
  } else if (!dif & !is.null(weights)) {
    responseMatrix <- cbind(personIds, fillerColumn, weights, fillerColumn, responseMatrix)    
    auxLength      <- maxLengthId + max(nchar(weights)) 
  } else if (!dif & is.null(weights)) {
    responseMatrix <- cbind(personIds, fillerColumn, responseMatrix)
    auxLength      <- maxLengthId 
  }

  # # Construct key file and omi file 
  keyLine        <- paste(c(rep(" ", auxLength), rep("1", nItems)), collapse = "")
  omitLine       <- paste(c(rep(" ", auxLength), rep("9", nItems)), collapse = "")
  notprLin       <- paste(rep(" ", auxLength + nItems), collapse = "")

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
  
  if (!is.null(datAnclas)) {
    cat("        PRNAME = '", prFileName, "', \n", sep = "",
        file = commandFile, append = TRUE)
    
    # # Position and code of items
    indPosi <- data.frame(item = itemIds,
                          pos  = seq(1, length(itemIds)))
    datAnclas <- subset(datAnclas, select = c("item", "dif", "disc", "azar"))
    indPosi   <- merge(indPosi, datAnclas, by = "item", all.x = TRUE)
    indPosi   <- cbind(indPosi, 'Fix' = ifelse(is.na(indPosi$dif), 0, 1))
    indPosi   <- indPosi[order(indPosi$pos), ]
    
    # # Construct PRM
    writePRM <- function(indPosi, prFile, isBad = NULL) {      
      if (!is.null(isBad)) {
        isElim  <- any(isBad %in% indPosi[["pos"]])
        if (isElim) {
          indPosi <- indPosi[-isBad, ]
          indPosi$pos <- 1:nrow(indPosi)
        }
      }
      prmDAT <- subset(indPosi, !is.na(dif), 
                      select = c("pos", "disc", "dif", "azar"))

      # # format and run
      cols   <- c("dif", "disc", "azar")
      for (jj in cols){
        prmDAT[[jj]] <- sprintf("%.6f", prmDAT[[jj]])
      }      
      # # output file
      write(nrow(prmDAT), file = prFile)
      write.table(prmDAT, file = prFile, append = TRUE, sep = "\t",
                  row.names = FALSE, na = " ", col.names = FALSE, 
                  quote = FALSE)
      if (verbose) cat("Prm file prepared\n")
      return(indPosi)
    }

    indPosi <- writePRM(indPosi, prFile)
  }
 
  cat("        NPArm = ", NPArm, ",\n", sep = "", file = commandFile, 
      append = TRUE)
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
  cat("      SCORE = '", toupper(scoreFileName), "'",
      sep = "", file = commandFile, append = TRUE)

  if (dif){
  cat(", \n", "      DIF = '", toupper(diffFileName), "'",
      sep = "", file = commandFile, append = TRUE)

  }
  cat(";\n", sep = "", file = commandFile, append = TRUE)

  #  cat("      COMBINE = '", combineFileName, "';\n", sep = "", file = commandFile, append = TRUE)
  cat(">LENGTH NITems = (", nItems, "); \n", sep = "", file = commandFile, append = TRUE)
  # # INPUT
  cat(">INPUT NTOTAL = ", nItems, ", \n", sep = "", file = commandFile, append = TRUE)
  if (dif) {
    cat("       NGRoup = ", length(table(group)), ", \n", sep = "", file = commandFile, append = TRUE)
    cat("       DIF, \n", sep = "", file = commandFile, append = TRUE)
  }
  cat("       KFNAME = '", keyFileName, "', \n", sep = "", file = commandFile, append = TRUE)
  cat("       OFNAME = '", omitkeyFileName, "', \n", sep = "", file = commandFile, append = TRUE)
  cat("       NFNAME = '", notpresFileName, "', \n", sep = "", file = commandFile, append = TRUE)
  cat("       NALt = 1000, \n", file = commandFile, append = TRUE)
  cat("       NIDCHAR = ", maxLengthId, "; \n", sep = "", file = commandFile, append = TRUE)
 

  # # ITEMS
  cat(">ITEMS INUMBERS = 1(1)", nItems, ", \n", sep = "", file = commandFile, append = TRUE)
  cat("       INAMES = (\n", file = commandFile, append = TRUE)
  auxItems <- strwrap(paste(itemIds, collapse = ", "), width = 80)
  cat(gsub("\\s", "", auxItems), sep = "\n", file = commandFile, append = TRUE)
  cat("                );\n", sep = "", file = commandFile, append = TRUE)

  # # TEST
  cat(">TEST \n", file = commandFile, append = TRUE)
  printFix <- function(vecFix, commandFile){  
      cat("      FIX = (\n", file = commandFile, append = TRUE)
      auxFix <- strwrap(paste(vecFix, collapse = ", "), width = 30)
      cat(gsub("\\s", "", auxFix), sep = "\n", file = commandFile, append = TRUE)
      cat("                ),\n", file = commandFile, append = TRUE)
  }

  if (!is.null(datAnclas)) {
    printFix(vecFix = indPosi$Fix, commandFile)
  }
  
  cat("      TNAME = '", substr(gsub("_V.+", "", runName), 1, 8), "', \n",
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
  cat("                );\n", file = commandFile, append = TRUE)

  if (dif) {
    nGroups <- length(table(group))
    for (ii in 1:nGroups){
      cat(">GROUP", ii, "  GNAme = '", groupLabel[ii], "';\n", sep = "", 
          file = commandFile, append = TRUE)
    }
  }

  # if (!is.null(weights)) {
  #   cat(",\n", file = commandFile, append = TRUE)
  #   cat("       WEIGHT\n", sep = "", file = commandFile, append = TRUE)
  # }
  #cat(";\n", file = commandFile, append = TRUE)

  # # (variable format statement)
  if (dif & !is.null(weights)) {
    cat("(", maxLengthId, "A1, I", groupWidth, ", 1F13.6,", nItems, "A1)\n",
        sep = "", file = commandFile, append = TRUE)
  } else if (dif & is.null(weights)) {
    cat("(", maxLengthId, "A1, I", groupWidth, ", ", nItems, "A1)\n", sep = "", file = commandFile, append = TRUE)
  } else if (!dif & !is.null(weights)) {
    cat("(", maxLengthId, "A1, 1F13.6, ", nItems, " A1)\n", sep = "", file = commandFile, append = TRUE)
  } else if (!dif & is.null(weights)) {
    cat("(", maxLengthId, "A1, ", nItems, "A1)\n", sep = "", file = commandFile, append = TRUE)
  }

  # # CALIB
  cat(">CALIB \n", file = commandFile, append = TRUE)
  if (!is.null(datAnclas)) {
    cat("       NOADJUST, \n", file = commandFile, append = TRUE)
  }
  cat("       CYCLES = 500, \n", file = commandFile, append = TRUE)
  cat("       NEWTON = 30, \n", file = commandFile, append = TRUE)
  cat("       NQPT = ", nQuadPoints, ", \n", sep = "", file = commandFile, append = TRUE) 
  
  if (flagRASCH) {
    cat("       RASCH, \n", file = commandFile, append = TRUE)
  }

  if (!flagSPrior) {
    cat("       NOSprior, \n", file = commandFile, append = TRUE)
  }
  
  if (flagTPrior) {
    cat("       TPRIOR, \n", file = commandFile, append = TRUE)
    cat("       READPRI, \n", file = commandFile, append = TRUE)
  }
  
  if (dif){
    cat("       REFERENCE = 1, \n", file = commandFile, append = TRUE)
  }
  #cat("       TPRIOR, \n", file = commandFile, append = TRUE)
  cat("       DIAGNOSIS = 1, \n", file = commandFile, append = TRUE)
  cat("       CRIT = 0.0001 \n", file = commandFile, append = TRUE)
  cat("       ;\n", file = commandFile, append = TRUE)

  # # SCORE
  if (flagTPrior){
    cat(">PRIOR TSIGMA=(3.0(0)", nItems, ");\n", file = commandFile, append = TRUE)
  }
  
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

  # # Escribiendo bats
  cat(paste(gsub("/", "\\\\", file.path(binPath, "BLM1.eje")),
            " ", runName, " > ", paste0(runName, ".log1"),
            "\n", sep = ""), file = gsub("\\.bat", "_f1.bat", batDir))

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

  # # Exclusión correlación biserial
  setwd(runPath)
  iiCor <- 1
  repeat{
    system("WinXP-RUNBILOG.bat")
    if (iiCor == 1){
      file.copy(tctFileName, gsub("\\.TCT", "_ori.TCT", tctFileName))
    }

    # # Identificando items malos
    auxTCT <- ReadBlTCTFile(tctFileName, ".")

    if (iiCor > 1) { # Despues de eliminar negativos
      auxTCT <- auxTCT[order(auxTCT[, "BISERIAL"]), ]
      isBad  <- auxTCT[, "BISERIAL"] < thrCorr
      isBad <- which(itemIds %in% auxTCT[isBad, "item"][1])
    } else { # Primera corrida eliminar negativos
      isBad  <- auxTCT[, "BISERIAL"] < 0
      isBad  <- which(itemIds %in% auxTCT[isBad, "item"])
    }   
    auxBlm <- readLines(commandFile)
    linea1 <- grep(pattern = "INUmber", auxBlm)
    linea2 <- min(grep(pattern = ">(CALIB|GROUP)", auxBlm))
    linea4 <- grep(pattern = ">TEST", auxBlm)
    
    # # Construyendo el nuevo archivo
    auxdata1 <- auxBlm[(linea1 + 1):(linea2 - ifelse(dif,2,3) )]
    auxdata <- unlist(strsplit(paste0(auxdata1, collapse=""), ","))
    auxdata <- strwrap(paste(subset(auxdata, !(auxdata %in% isBad)), 
                       collapse = ", "), width = 30)
    numAux  <- linea2 - linea1 - ifelse(dif,2,3) - length(auxdata)
    auxdata <- c(auxdata, rep("", numAux))
    auxBlm[(linea1 + 1):(linea2 - ifelse(dif,2,3))] <- gsub("\\s", "", auxdata)
    isBlanck <- grep("^$", auxBlm)
    isBlanck <- isBlanck[isBlanck > linea1 + 1 & isBlanck < linea2 - ifelse(dif,2,3)]
    if (length(isBlanck) > 0) {
      auxBlm   <- auxBlm[-isBlanck]  
    }
    
    # # Cambiar el NITems
    linea3 <- grep(pattern = "NITems", auxBlm)
    antNUM <- gsub(".+\\((\\d+)\\).+", "\\1", auxBlm[linea3])
    nueNUM <- as.character(as.numeric(antNUM) - length(isBad))
    nueNUM <- gsub("(.+\\()(\\d+)(\\).+)", paste0("\\1", nueNUM, "\\3"), 
                  auxBlm[linea3])
    auxBlm[linea3] <- nueNUM
    if (!is.null(datAnclas) & length(isBad) > 0) {
      # # Ajustando campo Fix
      cat(auxBlm[1:linea4], sep = "\n", file = commandFile)  
      #print(indPosi$Fix)
      #print(str(indPosi))
      cat("Eliminando items ...... \n")
      cat(isBad, "\n")
      #print(indPosi$Fix[-isBad])
      printFix(vecFix = indPosi$Fix[-isBad], commandFile)
      cat(auxBlm[(linea1 - 1):length(auxBlm)], sep = "\n", 
          file = commandFile, append = TRUE)
      #indPosi$Fix <- indPosi$Fix[-isBad]
      #readline(prompt="Press [enter] to continue")
      # # Ajustando archivo PRM
      indPosi <- writePRM(indPosi, prFile, isBad = isBad)
    } else {
      cat(auxBlm, sep = "\n", file = commandFile)  
    }

    # # revisar SCO 
    sizeSCO <- file.info(scoreFileName)$size
    if(sizeSCO >= 0 & all(auxTCT[, "BISERIAL"] > 0)){
      break
    }
    iiCor <- iiCor + 1
  }
  # if (runProgram) {
  #   system("WinXP-RUNBILOG.bat")
  # }
  setwd(srcPath)
}