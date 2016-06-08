
################################################################################
# # RunDimensionalityCC.R
# # R Versions: 3.02
# #
# # Author(s): Maria Fernanda Zarate Jimenez
# #
# # SABER 5° y 9° Factores Asociados
# # Description: Creates Excel outputs of dimensionality exporatory analysis for each
# #              block of items
# #
# # Inputs: datBlock and dictionaryList
# #
# # Outputs: 04Exploratorio PBA V.xlsx
# #          c("Rnw", "tex")) files
# #
# # File history:
# #   20111205: Creation
# #   20130125: Modification to implement in factors associated
# #   20140502: Modification to generated Excel outputs
################################################################################
# # Definition of input and output paths
################################################################################
options(encoding = "UTF-8")

################################################################################
# # global paths
################################################################################
# inPath        <- file.path("Input")
# funPath       <- file.path("Src", "Function")
# outPath       <- file.path("Output", "04Exploratorio")
# logPath       <- file.path("Log")
# outPathSamp   <- file.path("Output", "Muestras")
# inPathSam     <- file.path("Input", "Muestras")

###############################################################################
# # Load libraries
################################################################################
library(xtable)   # # 1.5-6
library(polycor)  # # 0.7-8
library(mc2d)     # # 0.1-12
library(ggplot2)  # # 0.8.9
library(lavaan)   # # 0.4-10
library(semTools)
library(car)
library(semPlot)  # # 0.2-8
library(GPArotation)
library(xlsx)
library(pcaPA)
library(reshape)
library(RColorBrewer) # # 1.0-5
library(kernlab)
library(HistogramTools)
library(cluster)

################################################################################
# # Load sourcefiles
################################################################################

source(file.path(funPath, "MeasurementModelsWithLavaan.R"))
source(file.path(funPath, "corItem.R"))
source(file.path(funPath, "log.R"))  # # log
source(file.path(funPath, "exploratoryFunctions.R")) 

################################################################################
# # Definition of class and parameters
################################################################################

# # Heritage class Analysis
Exploratory <- setClass("Exploratory", contains = "Analysis")
setMethod("initialize", "Exploratory", function(.Object, ..., param) {
	.Object@outFile$pathRdata <- "../Output/04Exploratorio/resulEXPO.Rdata"
	.Object <- callNextMethod()
  })


Exploratory <- function(test, paramExp = 
	                    list(kOmissionThreshold = 0.5,
						     flagOri = FALSE, flagTotal = TRUE,
						     flagSubCon = TRUE, orderedDat = TRUE,
						     catToNA = c('No Presentado', 'NR', 'Multimarca'),
						     seqFactors = NULL, rotation = 'oblimin',
						     semilla = format(Sys.time(), "%d%m%Y"),
						     tamSize = 0.5)){
  cat("Se correra un analisis exploratorio con los siguientes parametros: \n \n")
  print(paramExp)
  cat("\n----->")
  object <- new("Exploratory", test = test, param = paramExp)
  object <- filterAnalysis(object)
  return(object)
}

# # use parameter for correlations
useCorExp      <<- "complete.obs"
kThereLoadiExp <<- 0.15
nReplicatesExp <<- 200

# # Puntos de corte para colorear las comunalidades
kThresholdLoadSupExp <<- 0.60
kThresholdLoadMidExp <<- 0.30
kThresholdLoadInfExp <<- 0.15

# # Puntos de corte para colorear las cargas factoriales
kThresholdLoadNCExp  <<- 0.10
kThresholdComSupExp  <<- 0.60
kThresholdComMidExp  <<- 0.30
kThresholdComInfExp  <<- 0.15


###############################################################################
# # Definition of Analysis
################################################################################
setGeneric(name = "codeAnalysis", def = function(object){standardGeneric("codeAnalysis")})
setMethod("codeAnalysis", "Exploratory", 
function(object){
	source(file.path("Function", "exploratoryFunctions.R"))
    outPath  <- file.path(outPath, "04Exploratorio")
    if(dir.exists(outPath)){
    	cat("OJO-------> ya tenia el directorio 04Exploratorio creado\n")
    	unlink(outPath, recursive = TRUE)
    	dir.create(outPath, showWarnings = FALSE)
	}

	# # version with dict V00 and data _2014_01_28_17_10_35
	versionOutput <- object@verSalida
	versionComment <- paste0("Corrida Análisis Exploratorio --",  object@test@nomTest, "--") 

	# # version of input dictionary
	verDataIn <- object@test@verInput

	# # Dimensiones que quiere explorar el usuario
	flagUser <- !is.null(object@param$seqFactors)


	################################################################################
	# # Adjusting the DB
	###############################################################################

	outPathSamp <- file.path(outPath, "../Muestras")
  	if(!file.exists(outPathSamp)){
    	dir.create(outPathSamp)
    	cat("Se creo la carpeta muestras en el output!!!!!\n")
  	}
	################################################################################
	# # Analisis de Dimensionalidad
	################################################################################

	# # create list to save results
	listResults <- list()
	pruebasRead <- names(object@datAnalysis)

	for (kk in pruebasRead) {
	  # # create folder and routes to save results
	  # #  carpetas por prueba
	  auxPru 	 <- gsub("(::|\\s)","_", kk)
	  outPathPba <- file.path(outPath, auxPru)
	  # #  carpetas de muestras por prueba
	  outPathSamPba <- file.path(outPathSamp, auxPru)
	
	  if(!file.exists(outPathSamPba)){
	     dir.create(outPathSamPba, recursive = TRUE)
	  }
	  
	  outPathPbaGraph <- file.path(outPath, "graficas")
  	  if (!file.exists(outPathPbaGraph)) {
         dir.create(outPathPbaGraph, recursive = TRUE)
      }

	  # # keep items that aren't eliminated
	  dictVarPrueba <- object@datAnalysis[[kk]]$dictionary
  	  varId         <- dictVarPrueba[, 'id']
	  
	  if (is.null(object@datAnalysis[[kk]])) {
   	  	warning('No tiene Datos para hacer exploratory', kk)
   	  	next
  	  }

  	  # # Define la base de datos
  	  kkBlock  <- as.data.frame(object@datAnalysis[[kk]]$datos)
    	 
  	  # # Compute matriz correlation
  	  corBlock <- MakeCorrelation(kkBlock, outPathSamPba, verDataIn, auxPru, 
  	                			  semilla = object@param$semilla, 
  	 	            			  tamMue = object@param$tamSize, 
  	 	            			  varId = varId, useCor = useCorExp)
      
      corExpBlock  <- corBlock$corBlock
      expkkBlock   <- corBlock$sampleBlock
  
  
	  ###################################################
	  ### Obtain parallel analysis with exploratory data
	  ##################################################
	  paBlock <- PA(expkkBlock, percentiles = 0.95,
	                nReplicates = nReplicatesExp, type = "ordered",
	                use = useCorExp, algorithm = 'polychoric')
	  nFactors <- CountEigen.PA(paBlock)
  	  listResults[[auxPru]][["paBlock"]]  <- paBlock
  	  listResults[[auxPru]][["nFactors"]] <- nFactors
	  ###################################################
	  ### # # Obtain parallel analysis plot
	  ###################################################
  
	  paPlotFile <- file.path(outPath, 'graficas', paste("paParalel_", auxPru, "_V",
	                          versionOutput, ".png", sep = ""))
	  paPlot <- plot(paBlock, groupLabel = "",
	                 observed = "Observados",
	                 percentile = " percentil",
	                 xlab = "Eigenvalores ordenados",
	                 ylab = "Eigenvalores", main = "")
	  ggsave(paPlotFile)
	  listResults[[auxPru]][["paPlotFile"]] <- paPlotFile
  	    
	  ###################################################
	  ### code chunk number 7: optionalMultExpl
	  ###################################################
  
	  # # Sugerido por los eigenvalues
	  if(!flagUser){
	     if(nFactors == 1){
	        seqFactors <- c(nFactors, nFactors + 1)
	        resultsExp <- lapply(seqFactors, function(x)
	                          MakeExploratory(x, object@param$rotation,
	                        				  dictVarPrueba, corExpBlock))
	     } else{
	        seqFactors <- c(nFactors - 1, nFactors, nFactors + 1)
	        resultsExp <- lapply(seqFactors, function(x)
	                          MakeExploratory(x, object@param$rotation,
	                        				  dictVarPrueba, corExpBlock))
	     }
	     names(resultsExp) <- paste(seqFactors, 'Factores')
	  }

	  # Las dimensiones que quiera explorar el usuario
	  if(flagUser){
	    if(nFactors == 1){
	       resultsExp <- lapply(seqFactors, function(x) 
	     					  MakeExploratory(x, object@param$rotation,
	     									  dictVarPrueba, corExpBlock) )
	    } else{
	      resultsExp <- lapply(seqFactors, function(x)
	                          MakeExploratory(x, object@param$rotation,
	                        				  dictVarPrueba, corExpBlock) )
	    }
	  }
	  listResults[[auxPru]][["resultsExp"]] <- resultsExp
	  listResults[[auxPru]][["seqFactors"]] <- seqFactors
	}
	
	# # Guardando resultados 
    save(listResults, file = object@outFile$pathRdata)

})
################################################################################
# # Definition of output files
################################################################################

setGeneric(name = "outXLSX", def = function(object, ...){standardGeneric("outXLSX")})
setMethod("outXLSX", "Exploratory", 
function(object){
	outPath  <- file.path(outPath, "04Exploratorio")
	#####################################################
	# # Function to Make Cabezotes
	#####################################################
	flagUser <- !is.null(object@param$seqFactors)
	PutCabezote <-  function(nameSheet, object, kk, isCensal = TRUE){
	  codigo_prueba      <- gsub("^(.*)(::)(.*)","\\1", kk)
	  nItems <- nrow(object@datAnalysis[[kk]]$dictionary)
	  kOmissionThreshold <- object@param$kOmissionThreshold	
	  versionComment 	 <- gsub("^(.*)(::)(.*)","\\3", kk)
	  rotation           <- object@param$rotation	
	  nObsExploratory    <- nrow(object@datAnalysis[[kk]]$datos)
	  codigo_prueba2     <- object@test@nomTest

      # # Cabezote Exploratorio
	  isOmissDel <- kOmissionThreshold < 1 & kOmissionThreshold > 0
	  criterios1 <- c('Código de Prueba', 'Datos de Análisis',
	                  'Uso criterio Omisiones', 'Número de ítems',
	                  'Tipo de Análisis')

	  valores1 <-  data.frame(valor = c(codigo_prueba,
	                    ifelse(isCensal, 'Censal', 'Muestra Nacional'),
	                    ifelse(isOmissDel, 'Sí', 'No'),
	                    nItems, 'policoricas'))

	  cabBlock1 <- data.frame(criterios1, valores1)

	  addDataFrame(cabBlock1, sheet = get(nameSheet), startRow = 1,
	               startColumn = 1, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg))

	  criterios2 <- c('Prueba','n Análisis',
	                  'Criterio para tratamiento de omisiones',
	                  'Comentario', 'Rotación')

	  valores2   <-  data.frame(valor = c(codigo_prueba2,
	                            nObsExploratory, kOmissionThreshold,
	                            versionComment, rotation))

	  cabBlock2  <- data.frame(criterios2, valores2)

	  addDataFrame(cabBlock2, sheet = get(nameSheet), startRow = 1,
	               startColumn = 3, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg) )

	  criterios3 <- c('Selección de número de dimensiones',
	                  'Tratamiento de valores ausentes')

	  valores3   <- data.frame(valor = c(ifelse(flagUser, 'Usuario', 'Paralelo'),
	                          useCorExp))

	  cabBlock3  <- data.frame(criterios3, valores3)

	  addDataFrame(cabBlock3, sheet = get(nameSheet), startRow = 1,
	               startColumn = 5, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg))

	  criterios4 <- data.frame(loads = c(
	                           paste(kThresholdLoadSupExp, ' < |load| < 1.00'),
	                           paste(kThresholdLoadMidExp, ' < |load| < ', kThresholdLoadSupExp),
	                           paste(kThresholdLoadInfExp, ' < |load| < ',
	                                 kThresholdLoadMidExp)))

	  addDataFrame(criterios4, sheet = get(nameSheet), startRow = 3,
	               startColumn = 5, row.names = FALSE, col.names = FALSE)

	  colRetain  <- 5
	  rowRetain  <- 3:5
	  rows       <- xlsx::getRows(sheet = get(nameSheet), rowIndex = rowRetain )
	  cells      <- xlsx::getCells(rows, colIndex = colRetain)

	  posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	  pcl1       <- posiciones[1]
	  pcl2       <- posiciones[2]
	  pcl3       <- posiciones[3]

	  xlsx::setCellStyle(cells[[pcl1]], csL4)
	  xlsx::setCellStyle(cells[[pcl2]], csL3)
	  xlsx::setCellStyle(cells[[pcl3]], csL2)


	  criterios5 <- data.frame(commu = c(
	                           paste(kThresholdComSupExp, ' < comm < 1.00'),
	                           paste(kThresholdComMidExp, ' < comm < ',
	                                 kThresholdComSupExp),
	                           paste(kThresholdComInfExp, ' < comm < ',
	                                 kThresholdComMidExp)))


	  addDataFrame(criterios5, sheet = get(nameSheet), startRow = 3,
	               startColumn = 6, row.names = FALSE, col.names = FALSE)

	  colRetain  <- 6
	  rowRetain  <- 3:5
	  rows       <- xlsx::getRows(sheet = get(nameSheet), rowIndex = rowRetain )
	  cells      <- xlsx::getCells(rows, colIndex = colRetain)

	  posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	  pcl1       <- posiciones[1]
	  pcl2       <- posiciones[2]
	  pcl3       <- posiciones[3]

	  xlsx::setCellStyle(cells[[pcl1]], csC4)
	  xlsx::setCellStyle(cells[[pcl2]], csC3)
	  xlsx::setCellStyle(cells[[pcl3]], csC2)

	}
	load(object@outFile$pathRdata)
	pruebasRead <- names(object@datAnalysis)
	
	for (kk in pruebasRead) {
	  auxPru <- gsub("(::|\\s)","_", kk)
	  # # create folder and routes to save results
	  paBlock    <- listResults[[auxPru]]$paBlock
	  nFactors   <- listResults[[auxPru]]$nFactors
	  paPlotFile <- listResults[[auxPru]]$paPlotFile
	  resultsExp <- listResults[[auxPru]]$resultsExp
	  finseqFact <- listResults[[auxPru]]$seqFactors

      ########################################################
	  # #  SALIDAS A EXCEL
	  ########################################################
	  width              <- list()
	  colorLoadings      <- brewer.pal(3, "Blues")
	  colorCommunalities <- brewer.pal(3, "Greens")
	  colStyle           <- list()

	  wb <- xlsx::createWorkbook()

	  # # cell style
	  # # header style
	  csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) +
	            Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
	  # # merge style
	  csEnM <- CellStyle(wb) + Font(wb, isBold = TRUE) +
	            Alignment(h = "ALIGN_CENTER")
	  # # percentages style
	  csPor <- CellStyle(wb) + DataFormat("0.00%")
	  # # estilo de columnas que reportan la desviación estándar del por
	  csDs  <- CellStyle(wb) + DataFormat("(0.0%)") +
	            Alignment(v = "VERTICAL_CENTER") + Border()
	  csDsE <- CellStyle(wb) + Alignment(v = "VERTICAL_CENTER", wrapText = TRUE) +
	            Border()
	  # # estilo de columnas que reportan n
	  csN   <- CellStyle(wb) + DataFormat("0.000") + Font(wb, isItalic = TRUE)
	  csD   <- CellStyle(wb) + DataFormat("0.000")

	  csNE  <- CellStyle(wb) + Font(wb, isItalic = TRUE)
	  # # borde
	  csPC  <- CellStyle(wb) + Border() +
	            Alignment(v = "VERTICAL_CENTER", wrapText = TRUE)
	  # # fuente en negrilla
	  csNeg <- CellStyle(wb) + Font(wb, isBold = TRUE)
	  # # Columna centrada
	  cen   <- CellStyle(wb) + Alignment(h="ALIGN_CENTER")

	  # # estilo de loadings rango 0
	  csL0  <- CellStyle(wb) + DataFormat("0.000")
	  
	  # # estilo de loadings rango 1
	  csL1  <- CellStyle(wb) + DataFormat("0.000") + Font(wb, color = 9)
	  # # estilo de loadings rango 2
	  csL2  <- CellStyle(wb) + DataFormat("0.000") +
	             Fill(foregroundColor=colorLoadings[1])
	  # # estilo de loadings rango 3
	  csL3  <- CellStyle(wb) + DataFormat("0.000") +
	            Fill(foregroundColor=colorLoadings[2])
	  # # estilo de loadings rango 4
	  csL4  <- CellStyle(wb) + DataFormat("0.000") +
	            Fill(foregroundColor=colorLoadings[3])

	  # # estilo de comunalidad rango 1
	  csC1  <- CellStyle(wb) + DataFormat("0.000")
	  # # estilo de comunalidad rango 2
	  csC2  <- CellStyle(wb) + DataFormat("0.000") +
	             Fill(foregroundColor=colorCommunalities[1])
	  # # estilo de comunalidad rango 3
	  csC3  <- CellStyle(wb) + DataFormat("0.000") +
	            Fill(foregroundColor=colorCommunalities[2])
	  # # estilo de comunalidad rango 4
	  csC4  <- CellStyle(wb) + DataFormat("0.000") +
	            Fill(foregroundColor=colorCommunalities[3])


	  # # Salida de resúmen de los items
      varKeepDict   <- c('id', 'subCon', 'etiqu')
	  expItemIndex <- object@datAnalysis[[kk]]$dictionary[, varKeepDict]
      isVacio <- expItemIndex[, 'etiqu'] == '' | is.na(expItemIndex[, 'etiqu'])
	  if (any(isVacio)) {
	      expItemIndex[isVacio, 'etiqu'] <- expItemIndex[isVacio, 'instr']
	  }
      
      nameSheet     <- 'Items'
	  assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))
	  addDataFrame(expItemIndex, sheet = get(nameSheet), startRow = 7,
	               startColumn = 1, row.names = FALSE,
	               col.names = TRUE, colnamesStyle = csEnc)

	  xlsx::setColumnWidth(get(nameSheet), 1, 23)
	  xlsx::setColumnWidth(get(nameSheet), 2, 20)
	  xlsx::setColumnWidth(get(nameSheet), 3, 23)
	  xlsx::setColumnWidth(get(nameSheet), 4, 22)
	  xlsx::setColumnWidth(get(nameSheet), 5, 25)
	  xlsx::setColumnWidth(get(nameSheet), 6, 25)

	   
	  PutCabezote(nameSheet, object = object, kk = kk, isCensal = TRUE)

	  # # Grafica de sedimentación

	  if(!flagUser){
	     nameSheet <- 'Paralelo'
	  } else {
	    nameSheet <- 'Sedimentación'
	  }

	  assign(nameSheet , xlsx::createSheet(wb, sheetName = nameSheet ))
	  PutCabezote(nameSheet, object = object, kk = kk, isCensal = TRUE)

	  titleEigenvalues <- data.frame(tve = 'Eigenvalues')
	  addDataFrame(titleEigenvalues, sheet = get(nameSheet),
	  		       startRow = 7, startColumn = 2,
	               row.names = FALSE, col.names = FALSE,
	               colStyle = list('1' = csEnM))

	  addMergedRegion(sheet = get(nameSheet),
	                  startRow = 7, endRow = 7,
	                  startColumn = 2, endColumn = 6)

	  delta             <- as.numeric(c(' ', -diff(paBlock$observed$eigenValues, 1)))
	  varianceExplained <- paBlock$observed$eigenValues/
	                       sum(paBlock$observed$eigenValues)

	  eigenTable <- data.frame(Observed  = paBlock$observed$eigenValues,
	                           Simulated = paBlock$percentiles$eigenValues,
	                           Delta     = delta,
	                           VarianceExplained = varianceExplained,
	                           VarianceExplainedCum = cumsum(varianceExplained))
      names(eigenTable) <- c('Observed', 'Simulated', 'Delta',
                             '% Retained Variance', '% of cumulative variance retained')

	  addDataFrame(eigenTable, sheet = get(nameSheet), startRow = 9,
	               startColumn = 2, row.names = FALSE,
	               col.names = TRUE, colStyle = list('1' = csN, '2' = csN,
	               '3' = csN, '4' = csPor, '5'= csPor), colnamesStyle = csEnc)

	  paPlotPath <- file.path(outPath, "graficas", paste("paParalel_", auxPru, "_V",
	                          object@verSalida, ".png", sep = ""))
	  addPicture(file = paPlotPath,
	             sheet = get(nameSheet), scale = 1.0,
	             startRow = 9, startColumn = 7)
	  nFact <- data.frame(numF = 'nFactors', nFactors)
	  addDataFrame(nFact, sheet = get(nameSheet), startRow = 19,
	             startColumn = 20, row.names = FALSE,
	             col.names = FALSE,   colStyle = list('1' = csEnc))
	  for(ll in 1:6){
	      width[[ll]]    <- paste("xlsx::setColumnWidth(get(nameSheet),", ll, ",25)", sep='')
	      eval(parse(text = width[[ll]]))
	  }

	  ######################################################
	  # # Salida de los análisis por cada dimension
	  ######################################################

      loadingsList      <- list()
      communalitiesList <- list()
      correlationList   <- list()
      varExplainedList  <- list()
      seqRowVarExp      <- NULL
      nDims             <- length(finseqFact)
      widthRow          <- list()

	  for(jj in 1:nDims){
        nDim           <- finseqFact[jj]
        namesSheet     <- paste(nDim, "Dim", sep = '')
        assign(namesSheet, xlsx::createSheet(wb, sheetName = namesSheet))
        PutCabezote(namesSheet, object = object, kk = kk, isCensal = TRUE)
        varExplained  <- resultsExp[[jj]]$VarExplained
        correlation   <- resultsExp[[jj]]$Correlation
        loadings      <- resultsExp[[jj]]$Loadings
        communalities <- resultsExp[[jj]]$Communalities
        nRowOutVar <- nrow(varExplained)
        nColOutVar <- ncol(varExplained)

	    # # Varianza explicada

	    titleVarExplained <- data.frame(tve = 'Retained Variance')
	    addDataFrame(titleVarExplained, sheet = get(namesSheet),
			         startRow = 7, startColumn = 2,
	                 row.names = FALSE, col.names = FALSE,
	                 colStyle = list('1' = csEnM))

	    addMergedRegion(sheet = get(namesSheet),
	                    startRow = 7, endRow = 7,
	                    startColumn = 2, endColumn = 2 + nColOutVar - 1)

	    addDataFrame(varExplained, sheet = get(namesSheet), startRow = 9,
	                 startColumn = 2, row.names = FALSE,
	                 col.names = TRUE, colnamesStyle = csEnc,
	                 colStyle = list('1' = cen, '2' = csPor))

	    # # Correlaciones
        nRowOutCor <- 9 + nRowOutVar + 2
        nColOutCor <- ncol(correlation) + 1
        titleCorrelation <- data.frame(tc = 'Component Correlation Matrix')
        addDataFrame(titleCorrelation, sheet = get(namesSheet), startRow = nRowOutCor,
                     startColumn = 2, row.names = FALSE, col.names = FALSE,
                     colStyle = list('1' = csEnM))
        addMergedRegion(sheet = get(namesSheet),
                        startRow = nRowOutCor, endRow = nRowOutCor,
                        startColumn = 2, endColumn = ncol(correlation) + 2)

	    # # Agregar formato
	    temp <- 'eliminar'
	    for(ll in 1:ncol(correlation)){
	         colStyle[[ll]] <- paste("'", ll, "'=csC1", sep = '')
	         temp           <- paste(temp, colStyle[[ll]], sep=",")
	    }
	    temp1       <- "list("
	    temp2       <- ")"
	    temp3       <- gsub('eliminar,', '', temp)
	    colStyleDef <- paste(temp1, temp3, temp2)
	    addDataFrame(correlation, sheet = get(namesSheet),
	                startRow = nRowOutCor + 2, startColumn = 2,
	                row.names = TRUE, col.names = TRUE ,
	                colStyle = eval(parse(text = colStyleDef)) )
	    xlsx::setColumnWidth(get(namesSheet), 1, 23)
	    for(ll in 1:max(nColOutVar, nColOutCor)+1){
	       width[[ll]]    <- paste("xlsx::setColumnWidth(get(namesSheet),", ll, ",23)", sep='')
	       eval(parse(text = width[[ll]]))
	    }

	   # # Cargas factoriales
	   nColOutLoa    <- 2 + max(nColOutVar, nColOutCor) + 1
	   titleLoadings <- data.frame(tc = 'Rotate Loadings')
	   addDataFrame(titleLoadings, sheet = get(namesSheet), startRow = 7,
	                startColumn = nColOutLoa, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM))
	   addMergedRegion(sheet = get(namesSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = nColOutLoa,
	                   endColumn = nColOutLoa + ncol(loadings) - 1)
	   addDataFrame(loadings, sheet = get(namesSheet), startRow = 9,
	                startColumn = nColOutLoa, row.names = FALSE,
	                col.names = TRUE, colnamesStyle = csEnc)

	   # # Agregar color a las cargas
 
	   loadingsI  <- subset(loadings, select = -Item)
	   colRetain  <- seq(nColOutLoa + 1, nColOutLoa + nDim )
	   rowRetain  <- seq(10, 10 + nrow(loadings) - 1)
	   rows       <- xlsx::getRows(sheet = get(namesSheet),
	                             rowIndex = rowRetain )
	   cells      <- xlsx::getCells(rows, colIndex = colRetain)
	   posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	   ld0 <- abs(loadingsI) < kThresholdLoadInfExp & abs(loadingsI) > kThresholdLoadNCExp
	   ld1 <- abs(loadingsI) < kThresholdLoadNCExp
	   ld2 <- abs(loadingsI) > kThresholdLoadInfExp & abs(loadingsI) < kThresholdLoadMidExp
	   ld3 <- abs(loadingsI) > kThresholdLoadMidExp & abs(loadingsI) < kThresholdLoadSupExp
	   ld4 <- abs(loadingsI) >  kThresholdLoadSupExp
	   pcl0 <- posiciones[ld0]
	   pcl1 <- posiciones[ld1]
	   pcl2 <- posiciones[ld2]
	   pcl3 <- posiciones[ld3]
	   pcl4 <- posiciones[ld4]
	   lapply(pcl0, function(x) xlsx::setCellStyle(cells[[x]], csL0))
	   lapply(pcl1, function(x) xlsx::setCellStyle(cells[[x]], csL1))
	   lapply(pcl2, function(x) xlsx::setCellStyle(cells[[x]], csL2))
	   lapply(pcl3, function(x) xlsx::setCellStyle(cells[[x]], csL3))
	   lapply(pcl4, function(x) xlsx::setCellStyle(cells[[x]], csL4))

	   # # Comunalidades

	   nColOutCom  <- nColOutLoa + ncol(loadings) + 1
	   titleCommun <- data.frame(tcm = 'Communalities')
	   addDataFrame(titleCommun, sheet = get(namesSheet), startRow = 7,
	                startColumn = nColOutCom, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM),
	                colnamesStyle = csEnM)

	   addMergedRegion(sheet = get(namesSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = nColOutCom,
	                   endColumn = nColOutCom + ncol(communalities) - 1)

	   addDataFrame(communalities, sheet = get(namesSheet), startRow = 9,
	                startColumn = nColOutCom, row.names = FALSE,
	                col.names = TRUE, colnamesStyle = csEnc )

	   # # Agregar color a las comunalidades

	   communalitiesI <- subset(communalities, select = -Item)
	   colRetain      <- nColOutCom + 1
	   rowRetain      <- seq(10, 10 + nrow(communalities) - 1)
	   rows           <- xlsx::getRows(sheet = get(namesSheet),
	                                   rowIndex = rowRetain )
	   cells          <- xlsx::getCells(rows, colIndex = colRetain)
	   posiciones     <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	   
	   cg1  <- communalitiesI   < kThresholdComInfExp
	   cg2  <- kThresholdComInfExp < communalitiesI & communalitiesI <
	           kThresholdComMidExp
	   cg3  <- kThresholdComMidExp < communalitiesI & communalitiesI <
	           kThresholdComSupExp
	   cg4  <- kThresholdComSupExp < communalitiesI
	   pcg1 <- posiciones[cg1]
	   pcg2 <- posiciones[cg2]
	   pcg3 <- posiciones[cg3]
	   pcg4 <- posiciones[cg4]
       lapply(pcg1, function(x) xlsx::setCellStyle(cells[[x]], csC1) )
       lapply(pcg2, function(x) xlsx::setCellStyle(cells[[x]], csC2) )
       lapply(pcg3, function(x) xlsx::setCellStyle(cells[[x]], csC3) )
       lapply(pcg4, function(x) xlsx::setCellStyle(cells[[x]], csC4) )
       loadingsList[[jj]]      <- resultsExp[[jj]]$Loadings
       communalitiesList[[jj]] <- resultsExp[[jj]]$Communalities
       correlationList[[jj]]   <- resultsExp[[jj]]$Correlation
       varExplainedList[[jj]]  <- resultsExp[[jj]]$VarExplained
	 }



	  #################################################################
	  # # Salida de los resumenes de todas las dimensiones
	  #################################################################

	   nameSheet     <- 'Extracciones'
	   assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))
	   PutCabezote(nameSheet, object = object, kk = kk, isCensal = TRUE)

	   # # Varianzas Explicadas

	   nColOutCom    <- nColOutLoa + ncol(loadings) + 1
	   titleVariance <- data.frame(tvm = '% Retained Variance')
	   addDataFrame(titleVariance, sheet = get(nameSheet), startRow = 7,
	                startColumn = 2, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM) )

	   addMergedRegion(sheet = get(nameSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = 2, endColumn = 3)

	   nColVarExp      <- 2

	   varExplained    <- as.data.frame(t(sapply(varExplainedList, cbind)))
	   varExplained    <- apply(varExplained, 2, as.numeric)
	   colnames(varExplained) <- c('Component', 'Retained Variance')
	   nColVarExpCon   <- nrow(varExplained)

	   addDataFrame(varExplained, sheet = get(nameSheet),
	                 startRow = 9, startColumn = 2, row.names = FALSE,
	                 col.names = TRUE, colnamesStyle = csEnc,
	                 colStyle = list('2' = csPor) )


	    # # Correlaciones

	   seqRowCorr       <- sapply(correlationList, nrow)
	   seqColCorr       <- sapply(correlationList, ncol)
	   seqRowCorrCum    <- cumsum(seqRowCorr)
	   starRowCorr      <- 9 +  nColVarExpCon + 2

	   titleCorrelation <- data.frame(tvm = 'Component Correlation Matrix')
	   addDataFrame(titleCorrelation, sheet = get(nameSheet),
	                startRow = starRowCorr,
	                startColumn = 2, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM) )

	   addMergedRegion(sheet = get(nameSheet),
	                  startRow = starRowCorr, endRow = starRowCorr,
	                  startColumn = 2,
	                  endColumn = 2 + max(unlist(seqRowCorr)) )

	   for(jj in 1:nDims){

	       temp <- 'eliminar'

	        for(ll in 1:ncol(correlation)){
	            colStyle[[ll]] <- paste("'", ll, "'=csC1", sep = '')
	            temp           <- paste(temp, colStyle[[ll]], sep=",")
	        }

	          temp1       <- "list("
	          temp2       <- ")"
	          temp3       <- gsub('eliminar,', '', temp)
	          colStyleDef <- paste(temp1, temp3, temp2)


	      addDataFrame(correlationList[[jj]], sheet = get(nameSheet),
	                 startRow = starRowCorr  + seqRowCorrCum[jj] + jj,
	                 startColumn = 2, row.names = FALSE,
	                 col.names = TRUE, colnamesStyle = csEnc,
	                 colStyle = eval(parse(text = colStyleDef))
	                )

	   }


	  # # Comunalidades

	   nColOutComStart <- 1 +  max(seqColCorr[nDims], nColVarExp) + 2
	   nColOutComEnd   <- nColOutComStart + nDim
	   titleCommun     <- data.frame(tcm = 'Communalities')
	   addDataFrame(titleCommun, sheet = get(nameSheet), startRow = 7,
	                startColumn = nColOutComStart, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM) )

	   addMergedRegion(sheet = get(nameSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = nColOutComStart,
	                   endColumn = nColOutComEnd )

	   sumCommu   <- as.data.frame(communalitiesList)
	   sumCommuD  <- subset(sumCommu, select =
	                 grep('Extraction', colnames(sumCommu)))
	   colnames(sumCommuD) <- paste(finseqFact, 'Dimension', sep = '.')

	   addDataFrame(sumCommuD, sheet = get(nameSheet),
	                startRow = 9,
	                startColumn = nColOutComStart, row.names = TRUE,
	                col.names = TRUE, colnamesStyle = csEnc)

	   # # Agregar color a las comunalidades

        colRetain <- seq(nColOutComStart + 1,  nColOutComEnd)
        rowRetain <- seq(10, 10 + nrow(sumCommuD) - 1)
        rows      <- xlsx::getRows(sheet = get(nameSheet),
                                   rowIndex = rowRetain )
        cells     <- xlsx::getCells(rows, colIndex = colRetain)
        posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')

        cg1 <- sumCommuD  < kThresholdComInfExp
        cg2 <- kThresholdComInfExp < sumCommuD & sumCommuD < kThresholdComMidExp
        cg3 <- kThresholdComMidExp < sumCommuD & sumCommuD < kThresholdComSupExp
        cg4 <- kThresholdComSupExp < sumCommuD

        pcg1 <- posiciones[cg1]
        pcg2 <- posiciones[cg2]
        pcg3 <- posiciones[cg3]
        pcg4 <- posiciones[cg4]

        try(lapply(pcg1, function(x) xlsx::setCellStyle(cells[[x]], csC1) ), TRUE)
        try(lapply(pcg2, function(x) xlsx::setCellStyle(cells[[x]], csC2) ), TRUE)
        try(lapply(pcg3, function(x) xlsx::setCellStyle(cells[[x]], csC3) ), TRUE)
        try(lapply(pcg4, function(x) xlsx::setCellStyle(cells[[x]], csC4) ), TRUE)



	  for(ll in 1:nColOutComEnd){
	      width[[ll]]    <- paste("xlsx::setColumnWidth(get(nameSheet),", ll, ",18)", sep='')
	      eval(parse(text = width[[ll]]))
	  }


	   #############################################################
	   # # Ultima hoja: Resúmen de cargas factoriales
	   #############################################################

	   nameSheet <- 'Loadings'
	   assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))

	 # #  Agregar el titulo

	   loadingsTot <- as.data.frame(loadingsList)
	   expression  <- grep('Item\\.', colnames(loadingsTot))
	   loadingsTot[, expression] <- ''
	   names(loadingsTot) <- gsub("Item.[1-9]", ' ', names(loadingsTot))
	   names(loadingsTot) <- gsub("Factor\\.(\\d)(\\.\\d)*", "Factor \\1", names(loadingsTot))
	   names(loadingsTot) <- recode(names(loadingsTot), "NA = '' ")
	   
	      
	   auxTable    <- expItemIndex[, c("id", "etiqu")]
	   names(auxTable) <- gsub("^id$", 'Item', names(auxTable))
	   loadingsTot <- merge(auxTable, loadingsTot, by = "Item")


	   nColOutLoad   <- ncol(loadingsTot) + 1
	   titleLoadings <- data.frame(tlm = 'Rotate Loadings')
	   addDataFrame(titleLoadings, sheet = get(nameSheet), startRow = 7,
	                 startColumn = 2, row.names = FALSE,
	                 col.names = FALSE, colStyle = list('1' = csEnM) )

	   addMergedRegion(sheet = get(nameSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = 2, endColumn = nColOutLoad )


	   addDataFrame(loadingsTot, sheet = get(nameSheet),
	                startRow = 9, startColumn =  2,
	                row.names = FALSE, col.names =  TRUE,
	                colnamesStyle = csEnc, showNA = FALSE)

	   PutCabezote(nameSheet, object = object, kk = kk, isCensal = TRUE)

   	   # # Agregar color a las cargas

	    loadingsTotI <- loadingsTot[, -1]

		colRetain <- seq(3, ncol(loadingsTot) + 1)
		rowRetain <- seq(10, 10 + nrow(loadings) - 1)
		rows      <- xlsx::getRows(sheet = get(nameSheet),
	                      rowIndex = rowRetain )
		cells     <- xlsx::getCells(rows, colIndex = colRetain)

		posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')

	      ld0  <- (loadingsTotI > -kThresholdLoadInfExp & loadingsTotI < -kThresholdLoadNCExp) |
	             (loadingsTotI > kThresholdLoadNCExp   & loadingsTotI < kThresholdLoadInfExp)

	      ld1  <-  loadingsTotI < kThresholdLoadNCExp & loadingsTotI >
	                 -kThresholdLoadNCExp

	      ld2  <- (loadingsTotI <  kThresholdLoadMidExp & loadingsTotI >
	               kThresholdLoadInfExp) |
	             (loadingsTotI < -kThresholdLoadInfExp & loadingsTotI > -kThresholdLoadMidExp)

	      ld3  <- (loadingsTotI <  kThresholdLoadSupExp & loadingsTotI >
	                kThresholdLoadMidExp) |
	              (loadingsTotI < -kThresholdLoadMidExp & loadingsTotI >
	                -kThresholdLoadSupExp)
	 	ld4  <-  loadingsTotI < -kThresholdLoadSupExp | loadingsTotI > kThresholdLoadSupExp
	    ld5  <-  loadingsTotI == ''

		pcl0 <- posiciones[ld0]
		pcl1 <- posiciones[ld1]
	    pcl2 <- posiciones[ld2]
		pcl3 <- posiciones[ld3]
		pcl4 <- posiciones[ld4]
	    pcl5 <- posiciones[ld5]

	    try(lapply(pcl0, function(x) xlsx::setCellStyle(cells[[x]], csL0) ), TRUE)
	    
	    try(lapply(pcl1, function(x) xlsx::setCellStyle(cells[[x]], csL1) ), TRUE)
	    try(lapply(pcl2, function(x) xlsx::setCellStyle(cells[[x]], csL2) ), TRUE)
	    try(lapply(pcl3, function(x) xlsx::setCellStyle(cells[[x]], csL3) ), TRUE)
	    try(lapply(pcl4, function(x) xlsx::setCellStyle(cells[[x]], csL4) ), TRUE)
	    try(lapply(pcl5, function(x) xlsx::setCellStyle(cells[[x]], csL1) ), TRUE)



	    for(ll in 1:(max(nColOutLoad, 6)) ){
	      width[[ll]]    <- paste("xlsx::setColumnWidth(get(nameSheet),", ll, ",18)", sep='')
	      eval(parse(text = width[[ll]]))
	    }

	  outFile <- file.path(outPath,
	                       paste("04Exploratorio_", auxPru,"_V", 01,
	                             ".xlsx", sep = ''))
	  xlsx::saveWorkbook(wb, file = outFile)
	  cat("Termino Salida: ", outFile, "\n")
	}
})


setGeneric(name = "outHTML", def = function(object){standardGeneric("outHTML")})
setMethod("outHTML", "Exploratory", 
function(object){
	print("Funcion en construcción")
})