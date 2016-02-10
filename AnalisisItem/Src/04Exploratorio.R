################################################################################
# # RunDimensionalityCC.R
# # R Versions: 3.02
# #
# # Author(s): María Fernanda Zárate Jiménez
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
inPath        <- file.path("Input")
funPath       <- file.path("Src", "Function")
outPath       <- file.path("Output", "04Exploratorio")
logPath       <- file.path("Log")
outPathSamp   <- file.path("Output", "Muestras")
inPathSam     <- file.path("Input", "Muestras")

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

################################################################################
# # Load sourcefiles
################################################################################
setwd("\\\\192.168.200.228/academica$/SABER/SABER_2013/20132-EJERCICIO_ANALITEM359-JUNIO2")
source(file.path(funPath, "MeasurementModelsWithLavaan.R"))
source(file.path(funPath, "corItem.R"))
source(file.path(funPath, "univariateFunctions01.R"))
source(file.path(funPath, "log.R"))  # # log

################################################################################
# # Command line parameters
################################################################################
cat("-------------- Lectura de Archivos -----------------\n")
# # Lectura de parámetros
args <- commandArgs();

# #  check if --args used. This avoids a problem with earlier versions of R
argsPos  <- match("--args", args)
codeName <- gsub("--file=Src(\\\\)?", "", args[grep("--file", args)])

# #  Parameters extraction
if(!is.na(argsPos) && length(args) > argsPos){ 
  controlFile <- args[argsPos + 1];  # Class with parameters
  #controlFile <- "controlData.Rdata"; codeName = "04Exploratorio.R"
} else {
  cat("Parametros de la función:\n")
  cat("----> controlData: [Rdata] Class with parameters\n")
  stop("**ERROR**  en los parametros")
}

# # Cargando parametros de las pruebas
load(file.path(inPath, controlFile))

################################################################################
# # global definitions
################################################################################
setGeneric(name = "explorAnalysis", def = function(object){standardGeneric("explorAnalysis")})

setMethod("explorAnalysis", "Prueba", 
function(object){
	# # Tipo de Análisis
	isCensal <- FALSE
	
	# #  Tipo de Estudio
	isTypeB <- FALSE

	# # use parameter for correlations
	useCor      <- "pairwise.complete.obs"
	kThereLoadi <- 0.15
	nReplicates  <- 200

	# # use type of matrix
	useMatrixCorr <- 'policoricas'

	# # tipo de aplicacion 1 = Censal, 2 = Control, 3 = SobreMuestra,
	# # 4 = Especial, 5 = Adicional Censal, 6 = Adicional Control
	kApli <- c(2, 3, 4, 6)

	# # deleted students with more than 80% of omission for the topic
	# # in the items that are not eliminated
	kOmissionThreshold  <- 0.8

	# # Categories will consider as No Response
	catToNA <- c('No Presentado', 'NR', 'Multimarca')

	# # version with dict V00 and data _2014_01_28_17_10_35
	versionOutput <- object@verSalida
	versionComment <- paste0("Corrida Análisis Exploratorio --",  object@nomPrueba, "--") 

	# # cod for 'no eliminated' items
	kCodNElim <- '06'

	# # cod for 'paralel' items
	kCodPar   <- '01'

	# # cod for Graphs
	kExt <- '.png'

	logFile   <- file.path(logPath, "log.txt")

	# # version of input dictionary
	verDataIn <- object@verEntrada

	# # Flag para determinar quien escoge las dimensiones

	flagUser      <- FALSE

	# # Dimensiones que quiere explorar el usuario
	seqFactors    <- c(5, 6, 7, 8)

	# # Rotation to use in FA
	rotation <- 'oblimin'

	# # Puntos de corte para colorear las comunalidades

	kThresholdLoadSup <- 0.60
	kThresholdLoadMid <- 0.30
	kThresholdLoadInf <- 0.15

	# # Puntos de corte para colorear las cargas factoriales

	kThresholdLoadNC  <- 0.10
	kThresholdComSup  <- 0.60
	kThresholdComMid  <- 0.30
	kThresholdComInf  <- 0.15


	#####################################################
	# # Function to Make Cabezotes
	#####################################################
	PutCabezote <-  function(nameSheet){

	   # # Cabezote Exploratorio
	  namesPrueba <- subset(pruebasDesc, codigo_prueba == kk,
	                       select = c(codigo_prueba, prueba))


	  namesPrueba[, 'nItems'] <- nrow(expItemIndex)

	  criterios1 <- c('Código de Prueba', 'Datos de Análisis',
	                  'Uso criterio Omisiones', 'Número de ítems',
	                  'Tipo de Análisis')

	  valores1 <-  data.frame(valor = c(namesPrueba[, 'codigo_prueba'],
	                    ifelse(isCensal, 'Censal', 'Muestra Nacional'),
	                    ifelse(isOmissDel, 'Sí', 'No'),
	                    namesPrueba[, 'nItems'], useMatrixCorr ))

	  cabBlock1 <- data.frame(criterios1, valores1)

	  addDataFrame(cabBlock1, sheet = get(nameSheet), startRow = 1,
	               startColumn = 1, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg))


	  criterios2 <- c('Prueba','n Análisis',
	                  'Criterio para tratamiento de omisiones',
	                  'Comentario', 'Rotación')

	  valores2   <-  data.frame(valor = c(namesPrueba[, 'prueba'],
	                            nObsExploratory, kOmissionThreshold,
	                            versionComment, rotation))

	  cabBlock2  <- data.frame(criterios2, valores2)

	  addDataFrame(cabBlock2, sheet = get(nameSheet), startRow = 1,
	               startColumn = 3, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg) )

	  criterios3 <- c('Selección de número de dimensiones',
	                  'Tratamiento de valores ausentes')

	  valores3   <- data.frame(valor = c(ifelse(flagUser, 'Usuario', 'Paralelo'),
	                          useCor))

	  cabBlock3  <- data.frame(criterios3, valores3)

	  addDataFrame(cabBlock3, sheet = get(nameSheet), startRow = 1,
	               startColumn = 5, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg))

	  criterios4 <- data.frame(loads = c(
	                           paste(kThresholdLoadSup, ' < |load| < 1.00'),
	                           paste(kThresholdLoadMid, ' < |load| < ', kThresholdLoadSup),
	                           paste(kThresholdLoadInf, ' < |load| < ',
	                                 kThresholdLoadMid)))

	  addDataFrame(criterios4, sheet = get(nameSheet), startRow = 3,
	               startColumn = 5, row.names = FALSE, col.names = FALSE)

	  colRetain  <- 5
	  rowRetain  <- 3:5
	  rows       <- getRows(sheet = get(nameSheet), rowIndex = rowRetain )
	  cells      <- getCells(rows, colIndex = colRetain)

	  posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	  pcl1       <- posiciones[1]
	  pcl2       <- posiciones[2]
	  pcl3       <- posiciones[3]

	  setCellStyle(cells[[pcl1]], csL4)
	  setCellStyle(cells[[pcl2]], csL3)
	  setCellStyle(cells[[pcl3]], csL2)


	  criterios5 <- data.frame(commu = c(
	                           paste(kThresholdComSup, ' < comm < 1.00'),
	                           paste(kThresholdComMid, ' < comm < ',
	                                 kThresholdComSup),
	                           paste(kThresholdComInf, ' < comm < ',
	                                 kThresholdComMid)))


	  addDataFrame(criterios5, sheet = get(nameSheet), startRow = 3,
	               startColumn = 6, row.names = FALSE, col.names = FALSE)

	  colRetain  <- 6
	  rowRetain  <- 3:5
	  rows       <- getRows(sheet = get(nameSheet), rowIndex = rowRetain )
	  cells      <- getCells(rows, colIndex = colRetain)

	  posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	  pcl1       <- posiciones[1]
	  pcl2       <- posiciones[2]
	  pcl3       <- posiciones[3]

	  setCellStyle(cells[[pcl1]], csC4)
	  setCellStyle(cells[[pcl2]], csC3)
	  setCellStyle(cells[[pcl3]], csC2)

	}

	##############################################################
	# # Function to make parallel analysis
	##############################################################

	MakeParallel <- function(kFactors, rotation){

	  Items <- dictVarPruebaInd[, 'id']

	  blockPCA   <- princomp(covmat = corExpBlock$cor)

	  if(kFactors == 1){
	    rotation = ''
	  }

	  # # Cargas Factoriales
	    blockEFAP  <- paste(rotation,"(blockPCA$loadings[, 1:kFactors])", sep='')
	    blockEFA   <- eval(parse(text = blockEFAP))
	    if(kFactors == 1){
	     cargasFA   <- blockEFA
	    } else{
	      cargasFA   <- unclass(blockEFA$loadings)
	    }
	    cargasFAdf <- data.frame(Item = Items, cargasFA = cargasFA)
	    colnames(cargasFAdf) <- c('Item', paste("Factor", 1:kFactors))

	  # # Correlaciones
	  if(kFactors == 1){
	     compCorrelation <- as.matrix(1)
	  } else {
	     compCorrelation <- blockEFA$Phi
	  }
	  rownames(compCorrelation) <- paste("Comp.", 1:kFactors, sep = '')
	  colnames(compCorrelation) <- paste("Comp.", 1:kFactors, sep = '')



	 # # Comunalidades
	  if(kFactors == 1){
	     communalities <- as.vector(cargasFA^2)
	  } else{
	     common          <- blockEFA$loadings %*% t(blockEFA$loadings)
	     communalities   <- diag(common)
	  }
	  communalitiesdf <- data.frame(Item = Items, comunalidades = communalities)
	  colnames(communalitiesdf) <- c('Item', 'Extraction')


	  # # Varianza explicada

	  component <- kFactors
	  blockDesv <- blockPCA$sdev^2
	  blockVar  <- blockDesv[kFactors]
	  var       <- (blockDesv / sum(blockDesv))[kFactors]
	  cumVar    <- (cumsum(blockDesv)/sum(blockDesv))[kFactors]
	  expVar    <- data.frame(component, cumVar)
	  colnames(expVar) <- c('Component', '% Retained Variance')

	  parList <- list(Loadings = cargasFAdf,
	                  Communalities = communalitiesdf,
	                  Correlation = compCorrelation,
	                  VarExplained = expVar )

	  return(parList)

	}

	################################################################################
	# # load data
	################################################################################
	# # output from 00CrearRdata.R
	load(object@pathDic)
  	load(object@pathRdata)

	# # colocando en diccionario campo paralelo a 01
	dictionaryList$variables$paralelo <- "01"
	save(dictionaryList, file = object@pathDic)

	################################################################################
	# # Adjusting the DB
	###############################################################################

	# # conserved data from sample application
	if(object@exam == "ACC"){
		if(isTypeB){
		if (!is.data.frame(datBlock) & is.list(datBlock) & length(datBlock) == 1) {
		  datBlockControl <- list(subset(datBlock[[1]], x$tipoApli %in% kApli))
		  names(datBlockControl) <- names(datBlock)
		} else {
		  datBlockControl <- lapply(datBlock, function(x)
		                          subset(x, x$tipoApli %in% kApli))
		}
		}else{
			datBlockControl <- datBlock
		}
	} else {
    	datBlockControl <- lapply(datBlock, function(x) {
        	                      aux <- data.frame(x$calBlock)
            	                  names(aux) <- gsub("X(\\d+)", "\\1", names(aux))
                	              return(aux)})
  	}


	# # obtener los códigos de las pruebas leídas que corresponden a las
	# # pruebas que se desean analizar
	filtroDicc <- function(x, varsKeep) {
	    isNoElim    <- x[, 'elimina' ] == kCodNElim
	    isCodPar    <- x[, 'paralelo'] == kCodPar	    
	    if (object@exam == "ACC") {
	      isNI        <- x[, 'indice'  ] != 'NI' 
	      isPrueba    <- isNoElim & isCodPar & isNI
	    } else {
	      isPrueba    <- isNoElim & isCodPar
	    }     
	    return(x[isPrueba, varsKeep])
  	}

	pruebasDesc <- unique(filtroDicc(dictionaryList$variables, c('codigo_prueba', 'prueba')))
  	exist       <- pruebasDesc[, 'codigo_prueba'] %in% gsub("\\.con", "", names(datBlock))
  	pruebasRead <- pruebasDesc[exist, 'codigo_prueba']
  	pruebasRead <- sort(pruebasRead)

  	if(!file.exists(outPathSamp)){
    	dir.create(outPathSamp)
    	cat("Se creo la carpeta muestras en el output!!!!!\n")
  	}
	################################################################################
	# # Analisis de Dimensionalidad
	################################################################################

	# # create list to save results
	datBlockOmis <- list()

	for (kk in pruebasRead) {
	  # # create folder and routes to save results
	  # #  carpetas por prueba
	  outPathPba <- file.path(outPath, kk)
	  # #  carpetas de muestras por prueba
	  outPathSamPba <- file.path(outPathSamp, kk)
	  # #  carpetas de muestras completa
	  outPathtotal <- file.path(outPathSamPba, "total")

	  if(!file.exists(outPathSamPba)){
	     dir.create(outPathSamPba, recursive = TRUE)
	     dir.create(outPathtotal, recursive = TRUE)
	   #  dir.create(outPathtoInvar)
	     cat("Se creo la carpeta PBA",kk, "total y toInvariance en muestras\n")
	  }
	  
	  outPathPbaGraph <- file.path(outPath, "GRAPHS")
  	  if (!file.exists(outPathPbaGraph)) {
         dir.create(outPathPbaGraph, recursive = TRUE)
      }

	  # # keep items that aren't eliminated
	  dictVarPrueba    <- subset(dictionaryList$variables, codigo_prueba == kk)
  	  dictVarPruebaInd <<- filtroDicc(dictVarPrueba, names(dictVarPrueba))
  	  varId            <- dictVarPruebaInd[, 'id']

	  # # variables by index
	  dictKk <- subset(dictVarPruebaInd, TRUE, select = c(id, indice))
  	  if (any(!(dictKk[, 'id'] %in% names(datBlockControl[[paste0(kk, ".con")]])))) {
    		stop("No estan todas las variables de datBlock")
  	  }
	  
	  if (nrow(dictKk) == 0 & object@exam == "ACC") {
   	  	stop('No tiene Items para hacer la corrida en PBA', kk,
             'Revise si todos los Items estan eliminados o si tiene alguna
              escala diferente a NI')
  	  }

	  # # Recode 'Multimarca' 'No aplica' 'NR' for NA
	  datBlockOmis[[kk]]          <- datBlockControl[[paste0(kk, ".con")]]
	  if (object@exam == "ACC") {
    	datBlockOmis[[kk]][, varId] <- lapply(datBlockOmis[[kk]][, varId],
                                              RecodeToNA, catToNA)
    	datBlockOmis[[kk]] <- rename(datBlockOmis[[kk]], c(noHoja="consLect"))
  	  } else {
    	datBlockOmis[[kk]][, varId]  <- lapply(datBlockOmis[[kk]][, varId], function(x) ordered(x))
    	if (object@exam == "SABERPRO") {
      		datBlockOmis[[kk]] <- rename(datBlockOmis[[kk]], c(SNP="consLect"))
    	}
  	  }
	  kkBlock <- datBlockOmis[[kk]]
	  kkBlock <- datBlockOmis[[kk]][, c('consLect', varId)]

	  # # conserve rows with less than kOmissionThreshold NR data
	  isOmissDel <- kOmissionThreshold <= 1 & kOmissionThreshold > 0
	  if (isOmissDel) {
	    subCon     <- kkBlock[, varId]
	    subCon[, ] <- lapply(subCon, RecodeToNA, catToNA)
	    misRow     <- rowMeans(is.na(subCon))

	    isKeep     <- misRow <= kOmissionThreshold

	    kkBlock    <- kkBlock[isKeep, ]
	  }

	 # # Number of observations
	 nObsBlock <- nrow(kkBlock)

	 # # Obtain sample
	 confData <- file.path(outPathSamPba, "total",
	                       paste("confirmatorySample_", kk, "_V",
	                       verDataIn, ".RData", sep = ""))
     expData  <- file.path(outPathSamPba, "total",
                           paste("exploratorytorySample_", kk, "_V",
                           verDataIn, ".RData", sep = ""))

     if (!file.exists(confData) & !file.exists(expData)) {
    	isExploratory    <- sample(x = rownames(kkBlock), size = nObsBlock %/% 2)
    	expkkBlock       <- subset(kkBlock[isExploratory, ], select = -consLect)
    	rownames(expkkBlock) <- datBlockOmis[[kk]][isExploratory, ]$consLect
    	nObsExploratory  <- nrow(expkkBlock)

	    isConfirmatory        <- -as.numeric(isExploratory)
	    confkkBlock           <- subset(kkBlock[isConfirmatory, ], select = -consLect)
	    rownames(confkkBlock) <- kkBlock[isConfirmatory, ]$consLect
	    nObsConfirmatory      <- nrow(confkkBlock)

	    attr(expkkBlock, 'fechaDescarga')  <- attr(expkkBlock, 'fechaDescarga')
	    attr(expkkBlock, 'prueba')         <- attr(datBlock[[kk]], 'prueba')
	    attr(confkkBlock, 'fechaDescarga') <- attr(expkkBlock, 'fechaDescarga')
	    attr(confkkBlock, 'prueba')        <- attr(datBlock[[kk]], 'prueba')

	    save(expkkBlock, nObsExploratory, file = expData)
	    save(confkkBlock, nObsConfirmatory, file = confData)
	 } else {
    	load(expData)
  	 }

  	 # # Obtain correlation matrix
  	 corExpData   <- file.path(outPathSamPba, "total",
                               paste("corExploratory_", kk, "_V",
                               verDataIn, ".RData", sep = ""))
  	 corConfData  <- file.path(outPathSamPba, "total",
                               paste("corConfirmatory_", kk, "_V",
                               verDataIn, ".RData", sep = ""))

     if (!file.exists(corExpData) & !file.exists(corConfData)) {
     	corExpBlock  <- hetcor(expkkBlock, pd = TRUE, use = useCor, std.err =
                            FALSE, ML = FALSE)
     	corConfBlock <- hetcor(confkkBlock, pd = TRUE, use = useCor, std.err =
                            FALSE, ML = FALSE)

     	if(class(corConfBlock) != "try-error" & class(corExpBlock) != "try-error"){
      	  attr(corExpBlock, 'fechaDescarga')  <- attr(datBlock[[kk]], 'fechaDescarga')
    	  attr(corConfBlock, 'prueba')        <- attr(datBlock[[kk]], 'prueba')
          save(corExpBlock,  file = corExpData)
          save(corConfBlock, file = corConfData)
      	} else {
          cat("No se estimo la matriz policórica inicial\n\n")
          controlAnal@param$flagUni     <- FALSE
          controlAnal@param$flagMultiC  <- FALSE
          controlAnal@param$flagMultiNC <- FALSE
          controlAnal@param$flagBiFac   <- FALSE
        }
    } else {
      load(corExpData)
    }

	###################################################
	### Obtain parallel analysis with exploratory data
	##################################################
	paBlock <- PA(expkkBlock, percentiles = 0.95,
	              nReplicates = nReplicates, type = "ordered",
	              use = useCor, algorithm = 'polychoric')
	nFactors <- CountEigen.PA(paBlock)

	###################################################
	### # # Obtain parallel analysis plot
	###################################################

	paPlotFile <- file.path(outPath, 'GRAPHS', paste("paParalel_", kk, "_V",
	                        versionOutput, kExt, sep = ""))

	paPlot <- plot(paBlock, groupLabel = "",
	               observed = "Observados",
	               percentile = " percentil",
	               xlab = "Eigenvalores ordenados",
	               ylab = "Eigenvalores", main = "")
	ggsave(paPlotFile)

	###################################################
	### code chunk number 7: optionalMultExpl
	###################################################
	# # Salidas por dimension

	# Sugerido por los eigenvalues

	  if(!flagUser){
	     if(nFactors == 1){
	        seqFactors <- c(nFactors, nFactors + 1)
	        resultsExp <- lapply(seqFactors, function(x)
	                          MakeParallel(x, rotation) )
	     } else{
	        seqFactors <- c(nFactors - 1, nFactors, nFactors + 1)
	        resultsExp <- lapply(seqFactors, function(x)
	                          MakeParallel(x, rotation) )
	     }
	   names(resultsExp) <- paste(seqFactors, 'Factores')

	  }

	# Lo que quiera explorar el usuario

	  if(flagUser){
	    if(nFactors == 1){
	       resultsExp <- lapply(seqFactors, function(x) MakeParallel(x, rotation) )
	    } else{
	      resultsExp <- lapply(seqFactors, function(x)
	                          MakeParallel(x, rotation) )
	    }
	  }

	########################################################
	# #  SALIDAS A EXCEL
	########################################################
	width              <- list()
	colorLoadings      <- brewer.pal(3, "Blues")
	colorCommunalities <- brewer.pal(3, "Greens")
	colStyle           <- list()

	 wb <- createWorkbook()

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

	    varKeepDict   <- c('id', 'indice', 'etiqu')

	    expItemIndex <- dictVarPruebaInd[, varKeepDict]

	    isVacio <- expItemIndex[, 'etiqu'] == '' | is.na(expItemIndex[, 'etiqu'])
	    if (any(isVacio)) {

	      expItemIndex[isVacio, 'etiqu'] <- expItemIndex[isVacio, 'instr']
	    }

	   nameSheet     <- 'Items'
	   assign(nameSheet, createSheet(wb, sheetName = nameSheet))
	   addDataFrame(expItemIndex, sheet = get(nameSheet), startRow = 7,
	               startColumn = 1, row.names = FALSE,
	               col.names = TRUE, colnamesStyle = csEnc)

	    setColumnWidth(get(nameSheet), 1, 23)
	    setColumnWidth(get(nameSheet), 2, 20)
	    setColumnWidth(get(nameSheet), 3, 23)
	    setColumnWidth(get(nameSheet), 4, 22)
	    setColumnWidth(get(nameSheet), 5, 25)
	    setColumnWidth(get(nameSheet), 6, 25)

	    PutCabezote(nameSheet)

	# # Grafica de sedimentación

	 if(!flagUser){
	    nameSheet <- 'Paralelo'
	 } else {
	   nameSheet <- 'Sedimentación'
	 }

	    assign(nameSheet , createSheet(wb, sheetName = nameSheet ))
	    PutCabezote(nameSheet)

	    titleEigenvalues <- data.frame(tve = 'Eigenvalues')
	    addDataFrame(titleEigenvalues, sheet = get(nameSheet),
			     startRow = 7, startColumn = 2,
	                 row.names = FALSE, col.names = FALSE,
	                 colStyle = list('1' = csEnM))

	    addMergedRegion(sheet = get(nameSheet),
	                    startRow = 7, endRow = 7,
	                    startColumn = 2, endColumn = 6)

	    delta             <-  as.numeric(c(' ', -diff(paBlock$observed$eigenValues, 1)))
	    varianceExplained <- paBlock$observed$eigenValues/
	                                         sum(paBlock$observed$eigenValues)

	    eigenTable <- data.frame(Observed  = paBlock$observed$eigenValues,
	                             Simulated = paBlock$percentiles$eigenValues,
	                             Delta     = delta,
	                     VarianceExplained = varianceExplained,
	                  VarianceExplainedCum = cumsum(varianceExplained)
	                  )
	    names(eigenTable) <- c('Observed', 'Simulated', 'Delta',
	                          '% Retained Variance', '% of cumulative variance retained')

	    addDataFrame(eigenTable, sheet = get(nameSheet), startRow = 9,
	                 startColumn = 2, row.names = FALSE,
	                 col.names = TRUE, colStyle = list('1' = csN, '2' = csN,
	                 '3' = csN, '4' = csPor, '5'= csPor), colnamesStyle = csEnc)

	    paPlotPath <- file.path(outPath, "GRAPHS", paste("paParalel_", kk, "_V",
	                            versionOutput, kExt, sep = ""))
	    addPicture(file = paPlotPath,
	               sheet = get(nameSheet), scale = 1.0,
	               startRow = 9, startColumn = 7)
	    nFact <- data.frame(numF = 'nFactors', nFactors)
	    addDataFrame(nFact, sheet = get(nameSheet), startRow = 19,
	               startColumn = 20, row.names = FALSE,
	               col.names = FALSE,   colStyle = list('1' = csEnc))

	     for(ll in 1:6){
	         width[[ll]]    <- paste("setColumnWidth(get(nameSheet),", ll, ",25)", sep='')
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
	      nDims             <- length(seqFactors)
	      widthRow          <- list()

	   for(jj in 1:nDims){

	      nDim           <- seqFactors[jj]
	      namesSheet     <- paste(nDim, "Dim", sep = '')
	      assign(namesSheet, createSheet(wb, sheetName = namesSheet))
	      PutCabezote(namesSheet)

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

	      setColumnWidth(get(namesSheet), 1, 23)
	      for(ll in 1:max(nColOutVar, nColOutCor)+1){
	         width[[ll]]    <- paste("setColumnWidth(get(namesSheet),", ll, ",23)", sep='')
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
		rows       <- getRows(sheet = get(namesSheet),
	                           rowIndex = rowRetain )
		cells      <- getCells(rows, colIndex = colRetain)

		posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')

	      ld0 <- abs(loadingsI) < kThresholdLoadInf & abs(loadingsI) > kThresholdLoadNC
		ld1 <- abs(loadingsI) < kThresholdLoadNC
	      ld2 <- abs(loadingsI) > kThresholdLoadInf & abs(loadingsI) < kThresholdLoadMid
		ld3 <- abs(loadingsI) > kThresholdLoadMid & abs(loadingsI) < kThresholdLoadSup
	      ld4 <- abs(loadingsI) >  kThresholdLoadSup

	      pcl0 <- posiciones[ld0]
		pcl1 <- posiciones[ld1]
	      pcl2 <- posiciones[ld2]
		pcl3 <- posiciones[ld3]
		pcl4 <- posiciones[ld4]

	      lapply(pcl0, function(x) setCellStyle(cells[[x]], csL0) )
		lapply(pcl1, function(x) setCellStyle(cells[[x]], csL1) )
		lapply(pcl2, function(x) setCellStyle(cells[[x]], csL2) )
		lapply(pcl3, function(x) setCellStyle(cells[[x]], csL3) )
	      lapply(pcl4, function(x) setCellStyle(cells[[x]], csL4) )


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
		rows           <- getRows(sheet = get(namesSheet),
	                              rowIndex = rowRetain )
		cells          <- getCells(rows, colIndex = colRetain)

		posiciones     <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')

	      cg1  <- communalitiesI   < kThresholdComInf
	      cg2  <- kThresholdComInf < communalitiesI & communalitiesI <
	                kThresholdComMid
	      cg3  <- kThresholdComMid < communalitiesI & communalitiesI <
	                kThresholdComSup
		cg4  <- kThresholdComSup < communalitiesI

	      pcg1 <- posiciones[cg1]
	      pcg2 <- posiciones[cg2]
		pcg3 <- posiciones[cg3]
		pcg4 <- posiciones[cg4]

		lapply(pcg1, function(x) setCellStyle(cells[[x]], csC1) )
		lapply(pcg2, function(x) setCellStyle(cells[[x]], csC2) )
		lapply(pcg3, function(x) setCellStyle(cells[[x]], csC3) )
	      lapply(pcg4, function(x) setCellStyle(cells[[x]], csC4) )

	      loadingsList[[jj]]      <- resultsExp[[jj]]$Loadings
	      communalitiesList[[jj]] <- resultsExp[[jj]]$Communalities
	      correlationList[[jj]]   <- resultsExp[[jj]]$Correlation
	      varExplainedList[[jj]]  <- resultsExp[[jj]]$VarExplained

	  }



	  #################################################################
	  # # Salida de los resumenes de todas las dimensiones
	  #################################################################

	   nameSheet     <- 'Extracciones'
	   assign(nameSheet, createSheet(wb, sheetName = nameSheet))
	   PutCabezote(nameSheet)

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
	   colnames(sumCommuD) <- paste(seqFactors, 'Dimension', sep = '.')

	   addDataFrame(sumCommuD, sheet = get(nameSheet),
	                startRow = 9,
	                startColumn = nColOutComStart, row.names = TRUE,
	                col.names = TRUE, colnamesStyle = csEnc)

	# # Agregar color a las comunalidades

	    	colRetain <- seq(nColOutComStart + 1,  nColOutComEnd)
		rowRetain <- seq(10, 10 + nrow(sumCommuD) - 1)
		rows      <- getRows(sheet = get(nameSheet),
	                           rowIndex = rowRetain )
		cells     <- getCells(rows, colIndex = colRetain)

		posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')

	      cg1 <- sumCommuD  < kThresholdComInf
		cg2 <- kThresholdComInf < sumCommuD & sumCommuD < kThresholdComMid
	      cg3 <- kThresholdComMid < sumCommuD & sumCommuD < kThresholdComSup
		cg4 <- kThresholdComSup < sumCommuD

	      pcg1 <- posiciones[cg1]
	      pcg2 <- posiciones[cg2]
		pcg3 <- posiciones[cg3]
		pcg4 <- posiciones[cg4]

		try(lapply(pcg1, function(x) setCellStyle(cells[[x]], csC1) ), TRUE)
		try(lapply(pcg2, function(x) setCellStyle(cells[[x]], csC2) ), TRUE)
		try(lapply(pcg3, function(x) setCellStyle(cells[[x]], csC3) ), TRUE)
	      try(lapply(pcg4, function(x) setCellStyle(cells[[x]], csC4) ), TRUE)



	  for(ll in 1:nColOutComEnd){
	      width[[ll]]    <- paste("setColumnWidth(get(nameSheet),", ll, ",18)", sep='')
	      eval(parse(text = width[[ll]]))
	  }


	   #############################################################
	   # # Ultima hoja: Resúmen de cargas factoriales
	   #############################################################

	   nameSheet <- 'Loadings'
	   assign(nameSheet, createSheet(wb, sheetName = nameSheet))

	 # #  Agregar el titulo

	   loadingsTot <- as.data.frame(loadingsList)
	   expression  <- grep('Item\\.', colnames(loadingsTot))
	   loadingsTot[, expression] <- ''
	   names(loadingsTot) <- gsub("Item.[1-9]", ' ', names(loadingsTot))
	   names(loadingsTot) <- gsub("Factor\\.(\\d)(\\.\\d)*", "Factor \\1", names(loadingsTot))


	   names(loadingsTot) <- recode(names(loadingsTot), "NA = '' ")

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

	  PutCabezote(nameSheet)

	# # Agregar color a las cargas

	      loadingsTotI <- loadingsTot[, -1]

		colRetain <- seq(3, ncol(loadingsTot) + 1)
		rowRetain <- seq(10, 10 + nrow(loadings) - 1)
		rows      <- getRows(sheet = get(nameSheet),
	                      rowIndex = rowRetain )
		cells     <- getCells(rows, colIndex = colRetain)

		posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')

	      ld0  <- (loadingsTotI > -kThresholdLoadInf & loadingsTotI < -kThresholdLoadNC) |
	             (loadingsTotI > kThresholdLoadNC   & loadingsTotI < kThresholdLoadInf)

	      ld1  <-  loadingsTotI < kThresholdLoadNC & loadingsTotI >
	                 -kThresholdLoadNC

	      ld2  <- (loadingsTotI <  kThresholdLoadMid & loadingsTotI >
	               kThresholdLoadInf) |
	             (loadingsTotI < -kThresholdLoadInf & loadingsTotI > -kThresholdLoadMid)

	      ld3  <- (loadingsTotI <  kThresholdLoadSup & loadingsTotI >
	                kThresholdLoadMid) |
	              (loadingsTotI < -kThresholdLoadMid & loadingsTotI >
	                -kThresholdLoadSup)
	 	ld4  <-  loadingsTotI < -kThresholdLoadSup | loadingsTotI > kThresholdLoadSup
	      ld5  <-  loadingsTotI == ''

		pcl0 <- posiciones[ld0]
		pcl1 <- posiciones[ld1]
	      pcl2 <- posiciones[ld2]
		pcl3 <- posiciones[ld3]
		pcl4 <- posiciones[ld4]
	      pcl5 <- posiciones[ld5]

	     try(lapply(pcl0, function(x) setCellStyle(cells[[x]], csL0) ), TRUE)
	     try(lapply(pcl1, function(x) setCellStyle(cells[[x]], csL1) ), TRUE)
	     try(lapply(pcl2, function(x) setCellStyle(cells[[x]], csL2) ), TRUE)
	     try(lapply(pcl3, function(x) setCellStyle(cells[[x]], csL3) ), TRUE)
	     try(lapply(pcl4, function(x) setCellStyle(cells[[x]], csL4) ), TRUE)
	     try(lapply(pcl5, function(x) setCellStyle(cells[[x]], csL1) ), TRUE)



	      for(ll in 1:(max(nColOutLoad, 6)) ){
	          width[[ll]]    <- paste("setColumnWidth(get(nameSheet),", ll, ",18)", sep='')
	          eval(parse(text = width[[ll]]))
	      }

	  outFile <- file.path(outPath,
	                     paste("04Exploratorio_", kk,"_V", versionOutput,
	                           ".xlsx", sep = ''))

	  saveWorkbook(wb, file = outFile)
	}
})

################################################################################
# # Apply the exploratory function to each test in controlData
################################################################################

for (prueba in names(controlData)) {
  print(prueba)
  explorAnalysis(controlData[[prueba]])
}

