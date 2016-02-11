################################################################################
# # 00CrearRdata.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Alvaro Uzaheta
# #
# # SABER 5 y 9
# # Description: Read dictionary and data for FA study saving Rdata in the
# #               output directory
# #
# # Inputs: Information downloaded from AnalItem for students
# #         questionnaire
# #
# # Outputs: Rdata with dictionary information and Rdata with decode
# #           data for analyses 
# #
# # File history:
# #   20140128: Creation
# # 
# # ToDo:
# #       Include cases when the required topic doesn't have data 
# #       Include check for multiples main folders
# #       Include date and name of the dictionary in the attributes if
# #       those files
# #       Review the 'no present' in the ReadData 
################################################################################

options(encoding = "UTF-8")

################################################################################
# # Libraries
################################################################################
require(RODBC)    # # 1.3-3, Import xlsx
require(LaF)  # # 0.5, Fast access to large ASCII files
require(data.table)  # # 1.8.10, fast indexing, ordered joint, ...
require(xlsx) # # 0.5.5, export xlsx in a fun way
require(car) # # 2.0-19, recode


################################################################################
# # Definition of input and output paths
################################################################################
inPath  <- file.path("..","Input")
outPath <- file.path("..", "Output", "00Crear")
logPath <- file.path("..", "Log")

# # srcPath <- file.path("..","src")
funPath <- file.path("..", "..", "Src", "Function")

################################################################################
# # source of scripts with functions
################################################################################
source(file.path(funPath, "readFilesAI02.R"))  # # functions to read
source(file.path(funPath, "log.R"))  # # log


################################################################################
# # global definitions
################################################################################

# # date of the folder to use from AnalItem data
dateFolder   <- "_2014_02_07_11_00_45"

# # infomation about the dictionary to use
nameDictionary <- file.path(inPath, 
                            "Diccionario_EstudianteCensal_SABER5_00CC_v00.xlsx")                            
nameSheet      <- "diccionario00"  # # variables dictionary sheet

# # topics to read according with codigo_prueba in the dictionary
pruebas <- NULL

# # application kind: 1 = Censal, 2 = Control, 3 = SobreMuestra, 
# # 4 = Especial, 5 = Adicional Censal, 6 = Adicional Control
kApli <- c(2, 3, 4, 6)  # # consider values

# # deleted students with more than 80% of omission for the topic
# # omissionThreshold  <- 0.8 
# # flag items with correlations less than 0.2 with the index
# # kThresItemCor      <- 0.2 

# # name of log file
logFile <- file.path(logPath, "log.txt")

# # version with dict V00 and data _2014_02_07_11_00_45
versionOutput  <- "01"
versionComment <- "Salida de Creación de datos con la version de datos
2014_02_07 y diccionario version 00, corrida inicial y ajuste de ítems 
con problemas de diagramación"

# # name of Rdata files
datDictionary <- file.path(outPath, 
                           paste("dictionaryList_V",
                                 versionOutput, ".RData", sep = ""))

datReadBlock  <- file.path(outPath, 
                           paste("datBlock_V",
                                 versionOutput, ".RData", sep = ""))

################################################################################
# # functions
################################################################################

ReadForma <- function(x, dictionaryList, pruebasDesc, folderAre){

  isPrueba <- pruebasDesc[, 'codigo_prueba'] == x
  forma <- pruebasDesc[isPrueba, 'codigo_forma']

  regExp <- paste("PBA", forma, "$", sep = '') 

  inFolder <- grep(regExp, folderAre, value = TRUE)
  datBlock     <- ReadDataAI(folderName = inFolder, dict = dictionaryList, 
                             multiMarkOmiss = TRUE, verbose = TRUE, 
                             eliminatedVars = TRUE)
  return(datBlock)
}

################################################################################
# #  reading  dictionary and datblock of workspace
################################################################################

# # Reading the project dictionary    
dictionaryList <- ReadDict(fileName = nameDictionary, 
                           variables = nameSheet, 
                           categories = "OpcRespFinal", model = "model", 
                           index = "escalas", collapse = "colapsa", 
                           desElim = "elimina")


save(dictionaryList, file = datDictionary)
  
# # Reading the DB using complete items
     isNoElim    <- dictionaryList$variables[, 'elimina'] == '06'
     varsKeep    <- c('codigo_prueba', 'codigo_forma', 'prueba') 
     pruebasDesc <- unique(dictionaryList$variables[isNoElim, varsKeep])


if (is.null(pruebas)) {
  folder <- list.dirs(inPath) 
  folderAre <- grep(paste(dateFolder, ".+PBA\\d{3}$", sep = ''),
                    folder, value = TRUE)

  regExp <- paste("(.+", dateFolder, ")/.+", sep = '')
  folderMain <- unique(gsub(regExp, "\\1", folderAre))

  formasExist <- gsub(".+PBA(\\d{3})$", "\\1", folderAre)

  exist <- pruebasDesc[, 'codigo_forma'] %in% formasExist
  pruebasRead <- pruebasDesc[exist, 'codigo_prueba']
} 


# # 

if (length(pruebasRead) != 1) {
  datBlock <- sapply(pruebasRead, function(x) 
                     ReadForma(x, dictionaryList = dictionaryList,
                               pruebasDesc = pruebasDesc, 
                               folderAre = folderAre))
} else {
  lecPrueba <- ReadForma(pruebasRead, dictionaryList = dictionaryList,
                         pruebasDesc = pruebasDesc, 
                         folderAre = folderAre)
  datBlock  <- list(lecPrueba)
  names(datBlock) <- pruebasRead
}


################################################################################
# #  save data.frames
################################################################################
save(datBlock, file = datReadBlock)
     cat("%%% Guardando Datos")

# # log
RunLog()


