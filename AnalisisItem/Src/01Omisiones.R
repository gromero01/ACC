################################################################################
# # filename: 01Omisiones.R
# # R Versions: R version 3.0.2 i386
# #
# # Author(s): Ronald F. Herrera C.
# #    review: Mario Carrasco and Alvaro Uzaheta
# #
# # sb 5 y 9. 
# # Description: Describe the NA analyses for a data frame, where there NA or
# #               or multi marks.  
# # Inputs: Dictionary and list of data.frames with data from
# #         00CrearRdata.R 
# #
# # Outputs: Data frame (xlsx), a summary table for each block with pct of
# # complete cases and pct of omissions and pct of valid students. 
# # Also, it makes Densities and images plots to decide which
# # items to be marked. Make sure you start the procedure
# # using the original items without any colapse structure and passing the
# # Mutimark and Omission as NA data.      
# #
# # File history:
# #   Creation: 20111202
# #   Update: 20140201
# #   Creation of S4 class: 20160220
# # ToDo:
############################################################################

options(encoding = "UTF-8")

################################################################################
# # global paths 
################################################################################
# # docPath <- file.path("..","doc","latex")
inPath  <- file.path("..", "input")
funPath <- file.path("Function")
outPath <- file.path("..", "output", "01Omisiones")
logPath <- file.path("..", "log")


################################################################################
# # Sources
################################################################################
source(file.path(funPath, "univariateFunctions01.R"))
source(file.path(funPath, "log.R"))  # # log

################################################################################
# # Definition of class and parameters
################################################################################
outRdata     <- file.path("Outpu", "01Omisiones", "resulOmi.Rdata")
defaultParam <- list(kApli = c(2, 3, 4, 6),                     
                     kOmissionThreshold  =  0.8,                   
                     catToNA = c('NR', 'Multimarca'),
                     kCodNElim = '06')

setClass("Omisiones", contains =  "Analisis",
         prototype = list(prueba = NULL, param = defaultParam,  
                          outFile   = list(pathRdata = outRdata)), 
         validity  = function(object){               
           if (object@param$kOmissionThreshold < 0)
             return("Error en el parametro 'kOmissionThreshold' [0, 1]")
           if (object@param$kOmissionThreshold > 1)
             return("Error en el parametro 'kOmissionThreshold' [0, 1]")              
           return(TRUE)  
         })

setMethod("initialize", "Omisiones", function(.Object, ..., prueba) {
    if(missing(prueba)){
      stop("Se debe especificar un objeto 'prueba = '??? ")
    }
    .Object@prueba <- prueba
    callNextMethod(.Object, ..., prueba)    
  })

do.call(getIndex, c(list(tipo = "Ambos"), getParams(omi)))

getIndex(omi, tipo = "codigo_prueba", 
         kOmissionThreshold  =  0.8, 
         catToNA = c('NR', 'Multimarca'), kCodNElim = '06')

################################################################################
# # load data
################################################################################

setMethod("codeAnal", "Omisiones",
function(object){

################################################################################
# # Libraries
################################################################################
require(ggplot2)  # # 0.9.3.1
require(scales)  # # 0.2.3 
require(car)  # # 2.0-19
require(data.table)  # # 1.8.10
require(xlsx) # # 0.5.5


cat("Making the table with the NA analysis per block\n")

# # create list to save results
datBlockOmis <- list()
# # save xlsx
outFile <- file.path(outPath, 
                    paste("01omisiones_V", object@prueba@verSalida,
                     ".xlsx", sep = ''))
  
  # # Create excelworkBook
  wb <- createWorkbook()

  # # cell style
  # # header style
  csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) +
            Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
  # # percentages style
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

for (kk in pruebasRead) {
  
  # # keep items that aren't eliminated   
  dictVarPrueba <- subset(dictionaryList$variables, 
                          codigo_prueba == kk)
  dictVarPrueba <- dictVarPrueba[order(dictVarPrueba[, 'orden']), ]
  isNoElim <- dictVarPrueba[, 'elimina'] == kCodNElim
  varId    <- dictVarPrueba[isNoElim, 'id']

  # # variables by index
  dictKk <- subset(dictVarPrueba, isNoElim & indice != "NI",
                   select = c(id, indice))


  if (any(!(dictKk[, 'id'] %in% names(datBlockControl[[kk]])))) {
    stop("No estan todas las variables de datBlock")
  } 

  if (nrow(dictKk) == 0) {
    stop('No tiene ítems para hacer la corrida en PBA', kk,
         'Revise si todos los ítems estan eliminados o si tiene alguna
         escala diferente a NI')
  } 

  varsIndex <- split(dictKk[, 'id'], dictKk[, 'indice'])

  # # Recode 'Multimarca' 'No aplica' 'NR' for NA
  datBlockOmis[[kk]] <- datBlockControl[[kk]]
  datBlockOmis[[kk]][, varId] <- lapply(datBlockOmis[[kk]][, varId],
                                        RecodeToNA, catToNA)
  datBlockOmis[[kk]][, varId] <- droplevels(datBlockOmis[[kk]][, varId])
  
  # # Table 1 in the report
  tabMissBl <- lapply(varsIndex, CalculateMissing, 
                      datos = datBlockOmis[[kk]], 
                      omissionThreshold = kOmissionThreshold)
  tabMissBl <- MakeListData(tabMissBl, nvar = "vaAgrupacion")
  
  # # save table xls
    # # hoja 
    namesSheet <- paste('PBA', kk, sep = '')
    # # creación de una hoja
    assign(namesSheet, createSheet(wb, sheetName = namesSheet))

    nRow <- nrow(tabMissBl)
    addDataFrame(tabMissBl, sheet = get(namesSheet), startRow = 6,
                 startColumn = 1, row.names = FALSE,
                 col.names = FALSE, #colnamesStyle = csEnc,
                 colStyle = list('2' = csN, '5' = csN, '4' = csPor,
                                 '3' = csPor, '6' = csPor, 
                                 '7' = csN, '8' = csPor,
                                 '9' = csPor, '10' = csPor))
  
    setColumnWidth(get(namesSheet), 1, 15)
    setColumnWidth(get(namesSheet), 2, 15)
    setColumnWidth(get(namesSheet), 3, 10)
    setColumnWidth(get(namesSheet), 4, 10)
    setColumnWidth(get(namesSheet), 5, 10)
    setColumnWidth(get(namesSheet), 6, 10)
    setColumnWidth(get(namesSheet), 7, 10)
    setColumnWidth(get(namesSheet), 8, 10)
    setColumnWidth(get(namesSheet), 9, 10)
    setColumnWidth(get(namesSheet),10, 10)

    # # data.frame del titulo
    titulo <- data.frame(col1 = c('', 'Set Completo', '', '', 
                                  'Casos Completos', 
                                  '',  
                                  'Criterio de Omisiones',
                                  '', '', ''),
                         col2 = c('Escala', 'Casos', 'Mín Omiss', 'Máx Omiss',
                                  'N', '%',
                                  'N', '%', 'Mín Omiss', 
                                      'Máx Omiss'))

    addDataFrame(t(titulo), sheet = get(namesSheet), startRow = 4,
                 startColumn = 1, row.names = FALSE,
                 col.names = FALSE,
                 colStyle = list('1' = csEnc,  '2' = csEnc, 
                                 '3' = csEnc,  '4' = csEnc,
                                 '5' = csEnc,  '6' = csEnc, 
                                 '7' = csEnc,  '8' = csEnc,
                                 '9' = csEnc, '10' = csEnc))

   addMergedRegion(sheet = get(namesSheet), 
                   startRow = 4, endRow = 4,
                   startColumn = 2, endColumn =  4)
    
   addMergedRegion(sheet = get(namesSheet), 
                   startRow = 4, endRow = 4,
                   startColumn = 5, endColumn =  6)

   addMergedRegion(sheet = get(namesSheet), 
                   startRow = 4, endRow = 4,
                   startColumn = 7, endColumn = 10)

   # # 
   namesPrueba <- subset(pruebasDesc, codigo_prueba == kk, 
                        select = c(codigo_prueba, prueba))

   namesPrueba[, 'nItems'] <- ncol(datBlockControl[[kk]][, varId])
  
   addDataFrame(t(namesPrueba), sheet = get(namesSheet), startRow = 1,
                startColumn = 1, row.names = TRUE,
                col.names = FALSE, rownamesStyle = csNeg)
   titulo2 <- data.frame(descripcion = 'Corte Omisiones',
                        descripcio2 = 'Corte Omisiones',
                        valor = kOmissionThreshold)
   addDataFrame(titulo2, sheet = get(namesSheet), startRow = 1,
                startColumn = 4, row.names = FALSE,
                col.names = FALSE, 
                colStyle = list('1' = csNeg, '3' = csPor))

   addMergedRegion(sheet = get(namesSheet), 
                   startRow = 1, endRow = 1,
                   startColumn = 4, endColumn =  5)
    
   # # making 'images' plots to analyze NA data per item
   nOmit   <- rowSums(is.na(datBlockOmis[[kk]][, varId]))
   nOmisos <- colSums(is.na(datBlockOmis[[kk]][, varId]))
   labY    <- sequence(length(varId)) 
  
   outGraph   <- file.path(outPath, "graph",
                           paste("graImag_PBA", kk, "_", versionOutput, kExt,
                                  sep = '')) 

# #   outGraph80 <- file.path(outPath, 
# #                           paste("graImag80_PBA", kk, "_", versionOutput, kExt,
# #                                 sep = '')) 
  if (kExt == ".png") {
    png(file = outGraph, width = 900, height = 400)
  } else {
    postscript(file = outGraph, width = 11, height = 6.3, 
               horizontal = FALSE)
  } 

  image(y = labY,
        z = !is.na(datBlockOmis[[kk]][order(nOmit, decreasing = TRUE),
                   varId]),
        xlab = paste('Pba', kk), ylab = "Posición de los ítems", las = 1)
  dev.off()
  
  # # plot 
  outGraph1 <- file.path(outPath, "graph",
                          paste("graPropOmis01_PBA", kk, "_", 
                                versionOutput, kExt,
                                sep = '')) 

  
  outGraph2 <- file.path(outPath, "graph",
                          paste("graPropOmis02_PBA", kk, "_", 
                                versionOutput, kExt,
                                sep = '')) 

  nMeansOmit <- rowMeans(is.na(datBlockOmis[[kk]][, varId]))
  datBlockLOmiss <- datBlockOmis[[kk]][nMeansOmit <= kOmissionThreshold, ] 
  
  # # dotplots in the same order that in the questionnaire
  ptnItemOmiis <- colMeans(is.na(datBlockLOmiss))
  ptnItem    <- data.frame(ptnItemOmiis = ptnItemOmiis, 
                          id = names(ptnItemOmiis))
  ptnItem    <- merge(dictVarPrueba[, c("id", "indice", 'orden',
                                        'elimina')], ptnItem)
  ptnItem    <- subset(ptnItem, elimina == '06') 
  ptnItem    <- ptnItem[order(ptnItem[, "orden"]), ]
    
    figHistItMOm <- ggplot(ptnItem, aes(x = ptnItemOmiis, y = id)) + 
                    geom_point(aes(colour = indice))  
    figHistItMOm <- figHistItMOm + xlab("Omission Percentage") + 
                    ylab("Item") + labs(colour = "Index", 
                                        title = 'Omission by item')
    figHistItMOm <- figHistItMOm + scale_x_continuous(labels = percent)  
    ggsave(file = outGraph1, plot = figHistItMOm, width = 11)

    # # dotplots order by indice and pthItemOmiis
    ptnItem <- ptnItem[order(ptnItem[, "indice"], ptnItem[, "ptnItemOmiis"]), ]
    ptnItem[, "id"] <- ordered(ptnItem[, "id"], levels = ptnItem[, "id"])
    figHistItMOm <- ggplot(ptnItem, aes(x = ptnItemOmiis, y = id)) + 
                    geom_point(aes(colour = indice))  
    figHistItMOm <- figHistItMOm + xlab("Omission Percentage") + 
                    ylab("Item") + labs(colour = "Index", 
                                        title = 'Omission by item')
    figHistItMOm <- figHistItMOm + scale_x_continuous(labels = percent)

    ggsave(file = outGraph2, plot = figHistItMOm, width = 11)
    
    addPicture(file = outGraph2, sheet = get(namesSheet), scale = 0.5,
              startRow = nRow + 7, startColumn = 8)

    # # histogram omissions
    outGraph3 <- file.path(outPath,  "graph",
                          paste("graHistOmis_PBA", kk, "_", 
                                versionOutput, kExt,
                                sep = ''))

    pctnMiss  <- round(nOmisos, 3)
    # # revisar   
    indices <- dictVarPrueba[isNoElim, 'indice'] 
    pctnIndices <- as.data.frame(t(apply(is.na(datBlockOmis[[kk]][, varId]), 
                                         1, function (x) tapply(x, indices, 
                                                                mean))))
    rePctnIndices <- reshape(pctnIndices, direction = "long", 
                             varying = list(seq(ncol(pctnIndices))), 
                             v.names = "omiss")

    if(length(unique(indices)) != 1){
      rePctnIndices[, "time"] <- factor(rePctnIndices[, "time"], 
                                        labels = names(pctnIndices))
    } else{
      rePctnIndices[, "time"] <- unique(indices)
    }
    
    rePctnIndices[, "cuts"] <- cut(rePctnIndices[, "omiss"], 
                                   breaks = c(-Inf, 0, 0.2, 0.4, 0.6, 0.8, Inf),
                                   labels = c("0%" , ">0%-20%", "20%-40%",
                                              "40%-60%", "60%-80%", ">80%"))
    figPctnIndices <- ggplot(rePctnIndices, aes(x = cuts, fill=time)) + 
                      geom_bar(position = "dodge") 
    figPctnIndices <- figPctnIndices +  xlab("Omission Percentage") + 
                      ylab("Persons Evaluated") + 
                      labs(fill = "Index", title = 'Omission by index')
    if(length(unique(indices)) != 1){
        brewerplot <- function (palette) {
                figPctnIndices + scale_fill_brewer(palette = palette) 
        }
        brewerplot ("RdYlGn")
    } else{
        figPctnIndices
    }

    ggsave(file = outGraph3, plot = figPctnIndices, width = 11)
    addPicture(file = outGraph3, sheet = get(namesSheet), scale = 0.5,
              startRow = nRow + 7, startColumn = 1)

    
}

saveWorkbook(wb, file = outFile)
