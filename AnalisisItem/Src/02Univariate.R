################################################################################
# # filename: 02Univariate.R
# # R Versions: R version 3.0.2 i386
# #
# # Author(s): Carlos A. Arias R.
# #    review: MArio Carrasco and Alvaro Uzaheta
# #
# #  
# # Description: Built a data.frame with the percentages of response by
# #               category of response (Saber 5 y 9)  
# # Inputs: Dictionary and list of data.frames with data from
# #         00CrearRdata.R 
# #
# # Outputs: A xlsx file with the report of frecuencies and percentages
# #           by category  
# #
# # File history:
# #   Creation: 20111122 
# #     Update: 20140122
# # 
# # ToDo:
################################################################################

options(encoding = "UTF-8")

################################################################################
# # global paths 
################################################################################
# # docPath <- file.path("..","doc","latex")
inPath  <- file.path("..","input")
# # srcPath <- file.path("..","src")
funPath <- file.path("Function")
outPath <- file.path("..", "output", "02univariados")
logPath <- file.path("..", "log")

################################################################################
# # libraries
################################################################################
require(LaF)  # # 0.5
require(data.table)  # # 1.8.10
require(xlsx) # # 0.5.5
require(car)  # # 2.0-19

################################################################################
# # sources
################################################################################
source(file.path(funPath, "univariateFunctions01.R"))
source(file.path(funPath, "log.R"))  # # log
################################################################################
# # global definitions 
################################################################################
# # tipo de aplicacion 1 = Censal, 2 = Control, 3 = SobreMuestra, 
# # 4 = Especial, 5 = Adicional Censal, 6 = Adicional Control
kApli <- c(2, 3, 4, 6)

# # deleted students with more than 80% of omission for the topic 
# # in the items that are not eliminated
kOmissionThreshold  <- 0.8 

# # percentage where the category is considered with high omission
kOmisHigh <- 0.2

# # percentage where the category is considered low
kLowPer   <- 0.05

# # percentage where the category is considered high  
kHighPer  <- 0.95

# # flagCensal if TRUE get univariate from censal data
flagCensal <- FALSE

# # Categories will consider as No Response
catToNA <- c('NR', 'Multimarca')

# # Categories to drop when compute relative frecuencies
catToDrop <- c('No Presentado')
  
# # version with dict V00 and data _2014_01_28_17_10_35
versionOutput <- "01"
versionComment <- "Salida de univariados con la version de datos
2015_07_27 y diccionario version 00, corrida inicial"

# # cod for 'no eliminated' items
kCodNElim <- '06'

# # name of log file
logFile <- file.path(logPath, "log.txt")

# # version of input information
versionIn <- '01'

################################################################################
# # load data
################################################################################
# # output from 00CrearRdata.R
datDictionary <- file.path(inPath, "00Crear", 
                           paste("dictionaryList_V",
                                 versionIn, ".RData", sep = ""))
datReadBlock  <- file.path(inPath, "00Crear", 
                           paste("datBlock_V",
                                 versionOutput, ".RData", sep = ""))

load(datDictionary)
load(datReadBlock)

# # pesos
inFile <- file.path(inPath, 'pesosNacional.txt')
pesos <- read.table(inFile, sep = "\t", header = TRUE,
                    colClasses = c(rep('character', 2),
                                   rep('numeric', 5)))

################################################################################
# # Adjusting the DB
################################################################################

# # conserved data from sample application
if (!is.data.frame(datBlock) & is.list(datBlock) & length(datBlock) == 1) {
  datBlockControl <- list(subset(datBlock[[1]], tipoApli %in% kApli))
  names(datBlockControl) <- names(datBlock)
} else {
  datBlockControl <- lapply(datBlock, function(x) 
                          subset(x, x$tipoApli %in% kApli))
} 


# # temporal, para que sirve esa variable
dictionaryList$variables[, 'item'] <- '00'


# # obtener los códigos de las pruebas leídas que corresponden a las
# # pruebas que se desean analizar
     isNoElim    <- dictionaryList$variables[, 'elimina'] == kCodNElim
     varsKeep    <- c('codigo_prueba', 'codigo_forma', 'prueba') 
     pruebasDesc <- unique(dictionaryList$variables[isNoElim, varsKeep])

     exist <- pruebasDesc[, 'codigo_prueba'] %in% names(datBlock)
     pruebasRead <- pruebasDesc[exist, 'codigo_prueba']
     pruebasRead <- sort(pruebasRead)

################################################################################
# # generar univariados
################################################################################
# # variables to keep from dictionary in orderto complete the output
varKeepDict <- c('id', 'indice', 'instr', 'etiqu')

# # list con las salidas de univariados
univariados <- list()

# # guardar xlsx
outFile <- file.path(outPath, 
                     paste("univariados_V", versionOutput,
                           ".xlsx", sep = ''))

 wb <- createWorkbook()

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
  csN <- CellStyle(wb) + DataFormat("#,##0") + Font(wb, isItalic = TRUE)
  csNE <- CellStyle(wb) + Font(wb, isItalic = TRUE)
  # # borde
  csPC <- CellStyle(wb) + Border() +
          Alignment(v = "VERTICAL_CENTER", wrapText = TRUE)

  # # fuente en negrilla
  csNeg <- CellStyle(wb) + Font(wb, isBold = TRUE)

# # compute frecuencias by prueba
for (kk in pruebasRead) {
  # # keep items that aren't eliminated   
  dictVarPrueba <- subset(dictionaryList$variables, 
                          codigo_prueba == kk)
  isNoElim <- dictVarPrueba[, 'elimina'] == kCodNElim
  varId <- dictVarPrueba[isNoElim, 'id']
    
  # # conserve rows according to censal or sample application  
  isCensal <- is.null(flagCensal) | flagCensal
  if (is.null(flagCensal) | flagCensal) {
     kkBlock <- datBlock[[kk]]
     kkBlock[, 'peso'] <- 1  
   } else {
     kkBlock <- datBlockControl[[kk]]
     filas <- nrow(kkBlock)
     kkBlock <- merge(kkBlock, pesos[, c('consLect', 'peso')],
                      all.x = TRUE)
     nWPesos <- sum(!is.na(kkBlock[, 'peso']))  
     if (filas != nWPesos) {
       cat('pilas se pierden por pesos', filas - nWPesos,
           '\n')
     }   
   } 
  
  # # conserve rows with less than kOmissionThreshold NR data  
  isOmissDel <- kOmissionThreshold <= 1 & kOmissionThreshold > 0
  if (isOmissDel) {
    subCon <- kkBlock[, varId]
    subCon[, ] <- lapply(subCon, RecodeToNA, catToNA)
    misRow <- rowMeans(is.na(subCon))

    isKeep <- misRow <= kOmissionThreshold

    kkBlock <- kkBlock[isKeep, ]
  } 

  colmTomar   <- c("snp", "letraCuad", "grado", "tipoApli", 
                   "sejoId", "codMpio", "consLect", 'peso')
  colmTomar   <- names(kkBlock)[!(names(kkBlock) %in% colmTomar)]

  distribuciones <- MakeDistrData(datBlock = kkBlock[, colmTomar],
                          cateNA = catToNA, cateDrop = catToDrop,  
                          weight = kkBlock[, 'peso'],
                          kOmisHigh = kOmisHigh,
                          kLowPer = kLowPer, kHighPer = kHighPer)

  univar <- subset(distribuciones, !is.na(porcentaje),
                   select = c(codItem, opRes, frecuencia, porcentaje, 
                              porcentajePesos, marca))

  univar <- merge(univar, 
                  dictionaryList$variables[, varKeepDict],
                  by.x = 'codItem', by.y = 'id')
  isVacio <- univar[, 'etiqu'] == '' | is.na(univar[, 'etiqu'])
  if (any(isVacio)) {
    univar[isVacio, 'etiqu'] <- univar[isVacio, 'instr']
  } 
  
  names(univar)[9] <- "Item"
  
  univariados[[kk]] <- univar[, c(1, 7, 9, 2:6)]
  
  # # hoja 
  namesSheet <- paste('PBA', kk, sep = '')
  # # creación de una hoja


  assign(namesSheet, createSheet(wb, sheetName = namesSheet))

  # # poner el data.frame en la hoja
  addDataFrame(univariados[[kk]], sheet = get(namesSheet), startRow = 5,
               startColumn = 1, row.names = FALSE,
               col.names = TRUE, colnamesStyle = csEnc,
               colStyle = list('5' = csN, '6' = csPor, '7' = csPor))

  # # 
  namesPrueba <- subset(pruebasDesc, codigo_prueba == kk, 
                       select = c(codigo_prueba, prueba))

  namesPrueba[, 'nItems'] <- ncol(kkBlock[, colmTomar])
  
  addDataFrame(t(namesPrueba), sheet = get(namesSheet), startRow = 1,
               startColumn = 1, row.names = TRUE,
               col.names = FALSE, rownamesStyle = csNeg)

  setColumnWidth(get(namesSheet), 1, 15)
  setColumnWidth(get(namesSheet), 2, 15)
  setColumnWidth(get(namesSheet), 3, 20)
  setColumnWidth(get(namesSheet), 4, 20)
  setColumnWidth(get(namesSheet), 5, 11)
  setColumnWidth(get(namesSheet), 6, 11)
  setColumnWidth(get(namesSheet), 7, 16)
  setColumnWidth(get(namesSheet), 8, 20)

  # # 
  criterios <- data.frame(criterio = c('Tipo Análisis', 'Uso criterio omisiones'),
                          valor = c(ifelse(isCensal, 'Censal', 
                                           'Muestra Nacional'),
                                    ifelse(isOmissDel, 'Sí', 'No')))
  
  addDataFrame(criterios, sheet = get(namesSheet), startRow = 1,
               startColumn = 4, row.names = FALSE,
               col.names = FALSE, 
               colStyle = list('1' = csNeg))

  # # 
   tamAnal <- data.frame(criterio = c('n Análisis', 'n Pesos'),
                           valor = c(nrow(kkBlock),
                                     sum(!is.na(kkBlock[, 'peso'])))) 
  addDataFrame(tamAnal, sheet = get(namesSheet), startRow = 2,
               startColumn = 6, row.names = FALSE,
               col.names = FALSE, 
               colStyle = list('1' = csNeg))

}



   saveWorkbook(wb, file = outFile)

# # 
   outFile <- file.path(outPath, 
                        paste("univariados_V", versionOutput,
                           ".RData", sep = ''))
  save(univariados, file = outFile) 

# # log
RunLog()
 
