################################################################################
# # 03TCT.R
# # R Versions: R version 3.0.2 i386
# #
# # Author(s): Alvaro Uzaheta
# #
# # SABER 5° y 9°
# # Compute clasic statistics from TCT
# #
# # Inputs: Dictionary and list of data.frames with data from
# #         00CrearRdata.R
# #
# # Outputs: A xlsx file with the statistics from TCT
# #
# # File history:
# #   20111123: Creation
# #   20140211: Update
################################################################################

options(encoding = "UTF-8")
################################################################################
# # Definition of input and output paths
################################################################################
# inPath        <- file.path("Input")
# funPath       <- file.path("Src", "Function")
# outPath       <- file.path("Output", "03TCT")
# logPath       <- file.path("Log")


###############################################################################
# # Load wrapWS.R functions
################################################################################
source(file.path(funPath, "univariateFunctions01.R"))
source(file.path(funPath, "log.R"))  # # log
source(file.path(funPath, "plotCMC.R"))
source(file.path(funPath, "pruebaClass.R"))

################################################################################
# # Definition of class and parameters
################################################################################

# # Heritage class Analysis
TCT <- setClass("TCT", contains = "Analysis")

setMethod("initialize", "TCT", function(.Object, ..., param) {
  dir.create("../Output/03TCT/", showWarnings = FALSE, recursive = TRUE)
  .Object@outFile$pathRdata <- "../Output/03TCT/RDATA/outList_TCT.Rdata"
  .Object <- callNextMethod()
})

TCT <- function(test, paramExp = 
                      list(kOmissionThreshold = 0.5,
                           flagOri = FALSE, flagTotal = TRUE,
                           flagSubCon = TRUE, orderedDat = TRUE,
                           catToNA = c('No Presentado', 'NR', 'Multimarca'), 
                           isCheckKeys = FALSE)){
  cat("Se correra un analisis TCT con los siguientes parametros: \n")
  print(paramExp)
  cat("\n----->")
  object <- new("TCT", test = test, param = paramExp)
  object <- filterAnalysis(object)
  return(object)
}



setGeneric(name = "codeAnalysis", def = function(object){standardGeneric("codeAnalysis")})
setMethod("codeAnalysis", "TCT",
 # object <- controlData[[prueba]]
function(object){

    # # Load libraries
    require(psych)  # # 1.1.10
    require(ggplot2)  # # 0.9.3.1
    require(gtools)  # # 3.2.1
    require(LaF)  # # 0.5
    require(data.table)  # # 1.8.10
    require(xlsx)  # # 0.5.5

    # # version with dict V00 and data _2014_01_28_17_10_35
    versionOutput  <- object@verSalida
    versionComment <- paste0("Corrida Análisis TCT --",  object@test@nomTest, "--") 

    # # see ?psych::alpha
    isCheckKeys <- object@param$isCheckKeys

    # # version of input dictionary
    verDataIn <- object@test@verInput

    ################################################################################
    # # Analysis TCT
    ################################################################################
    
    # # create list to save results
    listResults <- list()
    pruebasRead <- names(object@datAnalysis)
    
    # # Extrae info sobre grado and test

    for (kk in pruebasRead) {
      # # create folder and routes to save results
      # #  carpetas por prueba
      auxPru   <- gsub("(::|\\s)","_", kk)
      outPathPba <- file.path(outPath, auxPru)      

      # # create folder and routes to save results
      outPathPba <- file.path(outPath, "03TCT")

      # # Create a directory in outPath
      dir.create(file.path(outPathPba, "graficos"), 
                 recursive = TRUE, showWarnings = FALSE)
      dictVarPrueba <- object@datAnalysis[[kk]]$dictionary
      indexNoNI     <- dictVarPrueba[, 'subCon']
      varId         <- dictVarPrueba[, 'id']
      dataBlo       <- as.data.frame(object@datAnalysis[[kk]]$datos)

      # # Consutrucción estadisticas clasicas
      dataCor     <- dataBlo[, varId]
      dataCor[, ] <- lapply(dataCor, as.numeric)
      corBlock    <- psych::alpha(dataCor,
                                  check.keys = isCheckKeys)$item.stats
      corBlock <- data.frame('id' = row.names(corBlock),
                             'corItBl' = corBlock[, "r.drop"])

      # # Graph CM Curve from source plotCMC.R
      outPathGraph <- file.path(outPathPba, "graficos")
      plotCMC(dataCor, outPathGraph, kk)
  
      # # compute correlations Item - Index
      varByIndex   <- split(varId, indexNoNI)
      corItemIndex <- lapply(varByIndex, function(x){
                             estIndex <- psych::alpha(dataCor[, x],
                                                      check.keys = isCheckKeys)  
                             estIndItems <- data.frame(estIndex$item.stats,
                                                       estIndex$alpha.drop)
        # #                  names(estIndItems)[4] <- "corItIn"
  
                             estIndItems[, 'id'] <- rownames(estIndItems)
                             varKeep <- c('id', 'r.drop', 'raw_alpha')#,
                                           #'G6.smc')
                             estIndItems <- estIndItems[, varKeep]
                             names(estIndItems)[2] <- c('corItIn')
                             estIndItems[, 'alphaTotal'] <-
                                   estIndex$total[, 'raw_alpha']
                             estIndItems[, 'nItems'] <- length(x)
                             return(estIndItems)})
      corItemIndex <- MakeListData(corItemIndex, nvar = "indice")
      corItemIndex <- merge(corItemIndex, corBlock)
  
      # # variables to keep from dictionary in orderto complete the output
      if (object@exam == "ACC") {
        varKeepDict <- c('id', 'instr', 'etiqu')
      } else {
        if (!"etiqu" %in% names(dictVarPrueba)) {
          dictVarPrueba[,"etiqu"] <-  dictVarPrueba[,"indice"]
        }
        varKeepDict <- c('id', 'etiqu')
      }
      corItemIndex <- merge(dictVarPrueba[, varKeepDict], corItemIndex)
      isVacio      <- corItemIndex[, 'etiqu'] == '' | is.na(corItemIndex[, 'etiqu'])
  
      if (any(isVacio)) {
        corItemIndex[isVacio, 'etiqu'] <- corItemIndex[isVacio, 'instr']
      }
      corItemIndex <- corItemIndex[, c("id", "indice", "etiqu", "alphaTotal", "raw_alpha",
                                       "corItBl", "corItIn", "nItems")]
        
      }
      datSblq <- rbind(datSblq, cbind(corItemIndex, 'pathCMC' = exGraphPath))
    }
  outList[[paste0(grado, ":", test)]] <<- datSblq
  saveWorkbook(wb, file = outFile)
})

################################################################################
# # Definition of output files
################################################################################

setGeneric(name = "outXLSX", def = function(object, ...){standardGeneric("outXLSX")})
setMethod("outXLSX", "TCT", 
function(object){
    # # guardar xlsx
    outFile <- file.path(outPathPba, 
                         paste("TCT_V", versionOutput, "_", 
                              test, grado, ".xlsx", sep = ''))
    wb      <- createWorkbook()

    # # estilo de las celdas
    # # estilo del encabezado
    csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) +
             Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
    # # estilo de columnas que reportan porcentajes
    csPor <- CellStyle(wb) + DataFormat("0.0%")
    # # estilo de columnas que reportan la desviación estándar del por
    csDs <- CellStyle(wb) + DataFormat("(0.0%)") +
            Alignment(v = "VERTICAL_CENTER") + Border()
    csDsE <- CellStyle(wb) + Alignment(v = "VERTICAL_CENTER", wrapText = TRUE) +
            Border()
    # # estilo de columnas que reportan n
    csN <- CellStyle(wb) + DataFormat("#,##0.00") + Font(wb, isItalic = TRUE)
    csNE <- CellStyle(wb) + Font(wb, isItalic = TRUE)
    
    # # borde
    csPC <- CellStyle(wb) + Border() +
            Alignment(v = "VERTICAL_CENTER", wrapText = TRUE)
    
    # # fuente en negrilla
    csNeg   <- CellStyle(wb) + Font(wb, isBold = TRUE)
    datSblq <- data.frame()

    # # creación de una hoja
    namesSheet <- kk
    assign(namesSheet, createSheet(wb, sheetName = namesSheet))
  
    # # poner el data.frame en la hoja
    addDataFrame(corItemIndex, sheet = get(namesSheet), startRow = 5,
                 startColumn = 1, row.names = FALSE,
                 col.names = TRUE, colnamesStyle = csEnc,
                 colStyle = list('4' = csN, '5' = csN, '6' = csN,
                                 '7' = csN, '8' = csN))
  
    # # Agrega el grafico CMC al excel
  
    exGraphPath <- grep(paste0("(.+CMC-", kk, "\\.png)"),
                        list.files(outPathGraph, full.names = TRUE),
                        value=TRUE)
    addPicture(exGraphPath, sheet = get(namesSheet), scale=1,
      startRow=5, startColumn=10)
  
    # #
    namesPrueba <- subset(pruebasDesc, codigo_prueba == kk,
                         select = c(codigo_prueba, prueba))
  
    namesPrueba[, 'nItems'] <- ncol(dataCor)
  
    addDataFrame(t(namesPrueba), sheet = get(namesSheet), startRow = 1,
                 startColumn = 1, row.names = TRUE,
                 col.names = FALSE, rownamesStyle = csNeg)
  
    setColumnWidth(get(namesSheet), 1, 15)
    setColumnWidth(get(namesSheet), 2, 15)
    setColumnWidth(get(namesSheet), 3, 10)
    setColumnWidth(get(namesSheet), 4, 10)
    setColumnWidth(get(namesSheet), 5, 10)
    setColumnWidth(get(namesSheet), 6, 10)
    setColumnWidth(get(namesSheet), 7, 10)
    setColumnWidth(get(namesSheet), 8, 10)
    setColumnWidth(get(namesSheet), 9, 5)

})

setGeneric(name = "outHTML", def = function(object){standardGeneric("outHTML")})
setMethod("outHTML", "TCT", 
function(object){
  print("Funcion en construcción")
})


setGeneric(name = "outXLSX", def = function(object){standardGeneric("outXLSX")})
setMethod("outXLSX", "TCT", 
function(object){
  print("Funcion en construcción")
})

setGeneric(name = "codeAnalysis", def = function(object){standardGeneric("codeAnalysis")})
setMethod("codeAnalysis", "TCT", 
function(object){
  print("Funcion en construcción")
})

