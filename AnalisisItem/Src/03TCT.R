################################################################################
# # 03TCT.R
# # R Versions: R version 3.2.3 x64
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
#setwd("\\\\icfesserv5\\academica$\\SABER\\SABER_2013\\20132-EJERCICIO_ANALITEM359-JUNIO2")
################################################################################
# # Definition of input and output paths
################################################################################
inPath        <- file.path("..", "Input")
funPath       <- file.path("Function")
outPath       <- file.path("..", "Output", "03TCT")
logPath       <- file.path("..", "Log")

################################################################################
# # Load libraries
################################################################################
require(psych)  # # 1.5.8
require(ggplot2)  # # 2.0.0
require(gtools)  # # 3.5.0
require(LaF)  # # 0.6.2
require(data.table)  # # 1.9.6
require(xlsx)  # # 0.5.7


###############################################################################
# # Load wrapWS.R functions
################################################################################
source(file.path(funPath, "univariateFunctions01.R"))
source(file.path(funPath, "log.R"))  # # log
source(file.path(funPath, "plotCMC.R"))
source(file.path(funPath, "pruebaClass.R"))


################################################################################
# # Command line parameters
################################################################################
# cat("-------------- Lectura de Archivos -----------------\n")
# # # Lectura de parámetros
# args <- commandArgs();

# # #  check if --args used. This avoids a problem with earlier versions of R
# argsPos  <- match("--args", args)
# codeName <- gsub("--file=Src(\\\\)?", "", args[grep("--file", args)])

# #  Parameters extraction
# if(!is.na(argsPos) && length(args) > argsPos){
#   controlFile <- args[argsPos + 1];  # Class with parameters
  controlFile <- "controlData.Rdata"; codeName = '03TCT.R'
# } else {
#   cat("Parametros de la función:\n")
#   cat("----> controlData: [Rdata] Class with parameters\n")
#   stop("**ERROR**  en los parametros")
# }

# # Cargando parametros de las pruebas
load(file.path(inPath, controlFile))

################################################################################
# # global definitions
################################################################################
setGeneric(name = "tctAnalysis", def = function(object){standardGeneric("tctAnalysis")})



setMethod("tctAnalysis", "Prueba",
 # object <- controlData[[prueba]]
 # object@Analisis$'03TCT.R'
function(object){
    controlPrueba <- object
    controlAnal   <- controlPrueba@Analisis[[codeName]]
    # # tipo de aplicacion 1 = Censal, 2 = Control, 3 = SobreMuestra,
    # # 4 = Especial, 5 = Adicional Censal, 6 = Adicional Control
    kApli <- controlAnal@param$kApli # c(2, 3, 4, 6)

    # # deleted students with more than 80% of omission for the topic
    # # in the items that are not eliminated, this analysis is with the
    # # complete data set
    kOmissionThreshold  <- controlAnal@param$kOmissionThreshold # 0.8

    # # flagCensal if TRUE get univariate from censal data
    # # this analysis is do it only with sample data
    # flagCensal <- FALSE

    # # Categories will consider as No Response
    catToNA <- controlAnal@param$catToNA # c('No Presentado', 'NR', 'Multimarca')

    # # version with dict V00 and data _2014_01_28_17_10_35
    versionOutput  <- paste0("0", controlPrueba@verSalida)
    # versionComment <- "Salida de TCT con la version de datos
    # 2015_07_27 y diccionario version 01, corrida inicial"

    # # extension to make the plots
    kExt <- ".png"

    # # cod for 'no eliminated' items
    kCodNElim <- controlAnal@param$kCodNElim

    # # flag items with correlations less than 0.2 with the index
    # kThresItemCor      <- 0.2

    # # see ?psych::alpha
    isCheckKeys <- controlAnal@param$isCheckKeys

    # # name of log file
    # logFile <- file.path(logPath, "log.txt")

    # # version of input information
    versionIn <- paste0("0", controlPrueba@verEntrada)

    ################################################################################
    # # load data
    ################################################################################
    # # output from 00CrearRdata.R
    load(controlPrueba@pathDic)
    load(controlPrueba@pathRdata)

    ################################################################################
    # # Adjusting the DB
    ################################################################################
    # # conserved data from sample application
    if(controlPrueba@exam == "ACC"){
      if (!is.data.frame(datBlock) & is.list(datBlock) & length(datBlock) == 1) {
        datBlockControl <- list(subset(datBlock[[1]], tipoApli %in% kApli))
        names(datBlockControl) <- names(datBlock)
      } else {
        datBlockControl <- lapply(datBlock, function(x)
                                subset(x, x$tipoApli %in% kApli))
      }
    } else {
      datBlockControl <- lapply(datBlock, function(x) {
                              aux <- data.frame(x$calBlock)
                              names(aux) <- gsub("X(\\d+)", "\\1", names(aux))
                              return(aux)})
    }

    # # Exclusión pruebas todo NI o todo Eliminado
    pruebasDicto <- dictionaryList$variables[, "prueba"]

    # # temporal, para que sirve esa variable
    dictionaryList$variables[, 'item'] <- '00'

    # # obtener los códigos de las pruebas leídas que corresponden a las
    # # pruebas que se desean analizar
    isNoElim    <- dictionaryList$variables[, 'elimina'] == kCodNElim
    
    if(controlPrueba@exam == "ACC"){
      varsKeep    <- c('codigo_prueba', 'codigo_forma', 'prueba')
      pruebasDesc <- unique(dictionaryList$variables[isNoElim, varsKeep])

      exist       <- pruebasDesc[, 'codigo_forma'] %in% gsub("(.+)(/PBA|/pba)(.+)", "\\3", names(datBlock))
      pruebasRead <- pruebasDesc[exist, 'codigo_prueba']
      pruebasRead <- sort(pruebasRead)
      # # Create the path
      outPathPba <- file.path(outPath, paste0("PBA", pruebasRead))
    } else {
      varsKeep    <- c('codigo_prueba', 'prueba')
      pruebasDesc <- unique(dictionaryList$variables[isNoElim, varsKeep])

      exist       <- pruebasDesc[, 'codigo_prueba'] %in% gsub("\\.con", "", names(datBlock))
      pruebasRead <- pruebasDesc[exist, 'codigo_prueba']
      pruebasRead <- sort(pruebasRead)
      # # Create the path 
      outPathPba <- file.path(outPath, gsub(".+\\\\(.+)$", "\\1", controlPrueba@path))
    }

    # # Extrae info sobre grado and test
    grado  <- gsub(".+GR\\.(\\d)(PBA|pba|sblq|SBLQ).+", "\\1", controlPrueba@pathDic)
    test   <- gsub("(PBA|pba|sblq|SBLQ)([A-Z]).+", "\\2", pruebasRead)[1]

    ################################################################################
    # # compute correlations item block and item index
    ################################################################################
    # # Create a directory in outPath
    if (!any(file.exists(outPathPba))) {
      if ( length(outPathPba > 1)) {
        apply(data.frame(outPathPba), 1, dir.create, recursive = TRUE)
      }
      dir.create(outPathPba, recursive = TRUE)
    }

    # # guardar xlsx
    outFile <- file.path(outPathPba, paste("TCT_V", versionOutput,
                         ".xlsx", sep = ''))
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
      csNeg <- CellStyle(wb) + Font(wb, isBold = TRUE)
      datSblq <- data.frame()

    for (kk in pruebasRead) {
     if (grepl("(pba|PBA)", kk) & controlPrueba@exam != "SABERPRO") {
       next
     } else {
      # # keep items that aren't eliminated
      dictVarPrueba <- subset(dictionaryList$variables,
                              codigo_prueba == kk)
      dictVarPrueba <- dictVarPrueba[order(dictVarPrueba[, 'orden']), ]
      isNoElim <- dictVarPrueba[, 'elimina'] == kCodNElim
      varId    <- dictVarPrueba[isNoElim, 'id']

      # # keep items that aren't belong to NI index
      if (object@exam == "ACC") {
        isNoNI    <- isNoElim
        varNoNI   <- dictVarPrueba[isNoNI, 'id']  
      } else {
        isNoNI    <- isNoElim & dictVarPrueba[, 'Indice'] != 'NI'
        varNoNI   <- dictVarPrueba[isNoNI, 'id']  
        indexNoNI <- dictVarPrueba[isNoNI, 'Indice']
      }
      

      # # recode to NA
      forma       <- pruebasDesc[ pruebasDesc[,"codigo_prueba"] == kk, "codigo_forma" ]
      dataBlo     <- datBlockControl[[paste0(kk, ".con")]][, varId]
      if (controlPrueba@exam == "ACC"){
        dataBlo     <- datBlockControl[[ grep(forma, names(datBlockControl), value = TRUE) ]]
        dataBlo     <- apply(dataBlo, 2, RecodeToNA, catToNA)
        dataBlo <- lapply(datBlockControl, RecodeToNA, catToNA)
      }

      # # conserve rows with less than kOmissionThreshold NR data
      isOmissDel <- kOmissionThreshold <= 1 & kOmissionThreshold > 0
      if (isOmissDel) {
        misRow  <- rowMeans(is.na(dataBlo))
        isKeep  <- misRow <= kOmissionThreshold
        dataBlo <- dataBlo[isKeep, ]
      }

      if (sum(isNoNI) != 0) {
        dataCor     <- dataBlo[, varNoNI]
        dataCor[, ] <- lapply(dataCor, as.numeric)
        corBlock    <- psych::alpha(dataCor,
                                check.keys = isCheckKeys)$item.stats
        corBlock <- data.frame('id' = row.names(corBlock),
                               'corItBl' = corBlock[, "r.drop"])
        # #     corBlock[, "id"] <- gsub("-", "", corBlock[, "id"])
        # #     outCorBlock <- rbind(outCorBlock, corBlock)
        # # Graph CM Curve from source plotCMC.R
        outPathGraph <- file.path(outPathPba, "graficos")
        plotCMC(dataCor, outPathGraph, kk)

        # # compute correlations Item - Index
        varByIndex   <- split(varNoNI, indexNoNI)
        corItemIndex <- lapply(varByIndex, function(x){
                                estIndex <- psych::alpha(dataCor[, x],
                                                         check.keys = isCheckKeys)

                                estIndItems <- data.frame(estIndex$item.stats,
                                                          estIndex$alpha.drop)
    # #                             names(estIndItems)[4] <- "corItIn"

                                estIndItems[, 'id'] <- rownames(estIndItems)

                                varKeep <- c('id', 'r.drop', 'raw_alpha')#,
                                             #'G6.smc')
                                estIndItems <- estIndItems[, varKeep]
                                names(estIndItems)[2] <- c('corItIn')

                                estIndItems[, 'alphaTotal'] <-
                                  estIndex$total[, 'raw_alpha']

                                estIndItems[, 'nItems'] <- length(x)
                                return(estIndItems)

                               })

        corItemIndex <- MakeListData(corItemIndex, nvar = "Indice")
        corItemIndex <- merge(corItemIndex, corBlock)

        # # variables to keep from dictionary in orderto complete the output
        if (controlPrueba@exam == "ACC") {
          varKeepDict <- c('id', 'instr', 'etiqu')
        } else {
          if (!"etiqu" %in% names(dictVarPrueba)) {
            dictVarPrueba[,"etiqu"] <-  dictVarPrueba[,"Indice"]
          }
          varKeepDict <- c('id', 'etiqu')
        }
        corItemIndex <- merge(dictVarPrueba[, varKeepDict], corItemIndex)
        isVacio <- corItemIndex[, 'etiqu'] == '' | is.na(corItemIndex[, 'etiqu'])

      if (any(isVacio)) {
        corItemIndex[isVacio, 'etiqu'] <- corItemIndex[isVacio, 'instr']
      }
      corItemIndex <- corItemIndex[, c("id", "Indice", "etiqu", "alphaTotal", "raw_alpha",
                                       "corItBl", "corItIn", "nItems")]

      # # ordenar total_alpha raw_alpha itPr itInd N
      namesSheet <- kk
      # # creación de una hoja
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
      }
    }
      cat("voy en la prueba", test, "del grado", grado, "\n")
      datSblq <- rbind(datSblq, cbind(corItemIndex, 'pathCMC' = exGraphPath))
    }
  outList[[paste0(grado, ":", test)]] <<- datSblq
  saveWorkbook(wb, file = outFile)
})

################################################################################
# # Apply the TCT function to each test in controlData
################################################################################
outList <<- list()
for (prueba in names(controlData)) {
  print(prueba)
  tctAnalysis(controlData[[prueba]])
}
save(outList, file = file.path(outPath, "outList_TCT.Rdata"))
save(controlData, file = file.path(inPath, controlFile))


