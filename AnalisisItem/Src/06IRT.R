###############################################################################
# # filename: 06IRT.R
# # R Versions: R version 3.0.2
# #
# # Author(s): Ronald Herrera, Álvaro Uzaheta and Víctor H. Cervantes
# #            (original runs Rnw files)
# #
# # SABER 5° y 9° Citizenship competencies
# # Description: Code to run IRT (Item Response Theory) models (GRM and
# #              2PL) through Parscale for blocks of items (index) for test
# #
# # Inputs: Dictionary and list of data.frames with data from
# #         00CrearRdata.R
# #
# # Outputs: If isReporte = FALSE file in xlsx with:
# #          - identification of items by block
# #          - table and graphic of Cronbach-Mesbah Curve
# #          - graphic of Item Characteristic Curve (ICC)
# #          - graphic of Items/Index Information Curve and
# #            tables with the approximate areas of the curves
# #          - graphic person-item map and table with
# #            the distribution of people by ability
# #          - table with estimated parameters
# #            and measures of goodness of fit
# #          If isReporte = TRUE previous graphs in outPath and
# #            file in txt with table with estimated parameters
# #            and measures of goodness of fit
# #
# # File history:
# #   20111214: Creation
# #   20130222: Adaptation for FA indices (Mario Carrasco)
# #   20140425: Adaptation for output in xlsx (Sandra Ropero)
# #   20140530: Adaptation for graphics and tables additional (Sandra Ropero)
# #   20140609: Adaptation for 2PL models, indexes with just three items
# #             and output when exists problems estimability of the parameters
# #             (Fabio Tejedor and Sandra Ropero)
# #   20151124: Adaptation for 2PL models with Bilog,
# #             and for Test of SABER 3, 5, 7, 9
# #             (Jorge Mario Carrasco Ortiz)
# #
# # ToDo: - implement to run models PCM, RSM, MRD
# #       - implement filters for disabled or other group of people
# #       - implement confidence intervals in graphic ICC
# #       - implement changes when indexes of different tests have the same name
################################################################################
# # global options
###############################################################################
options(encoding = "UTF-8")

################################################################################
# # Definition of input and output paths
################################################################################

inPath  <- file.path("Input")
outPath <- file.path("Output", "06IRT")
funPath <- file.path("Src", "Function")
logPath <- file.path("Log")

# # to run Winsteps and Parscale
binPath <- file.path("Src", "bin")

################################################################################
# # Load libraries
################################################################################
require(psych)  # # 1.1.10
require(ggplot2)  # # 0.9.3.1
require(gtools)  # # 3.2.1
require(scales)  # # 0.2.3
require(car)  # # 2.0-19
require(LaF)  # # 0.5
require(data.table)  # # 1.8.10
require(xlsx)  # # 0.5.5
require(RColorBrewer) # # 1.0-5
require(gridExtra)

###############################################################################
# # load functions
################################################################################
source(file.path(funPath, "univariateFunctions01.R"))
source(file.path(funPath, "log.R"))
source(file.path(funPath, "wrapWS.R"))
source(file.path(funPath, "wrapPS.R"))
source(file.path(funPath, "wrapBL.R"))
source(file.path(funPath, "plotICCP.R"))
source(file.path(funPath, "plotPImap.R"))
source(file.path(funPath, "plotPImap.R"))
source(file.path(funPath, "pruebaClass.R"))
source(file.path(funPath, "WrightMapICFES.R"))

################################################################################
# # Command line parameters
################################################################################
# cat("-------------- Lectura de Archivos -----------------\n")
# # # Lectura de parámetros
# args <- commandArgs();

# # #  check if --args used. This avoids a problem with earlier versions of R
# argsPos  <- match("--args", args)
# codeName <- gsub("--file=Src(\\\\)?", "", args[grep("--file", args)])

# # #  Parameters extraction
# if(!is.na(argsPos) && length(args) > argsPos){
#   controlFile <- args[argsPos + 1];  # Class with parameters
#   #controlFile <- "controlData.Rdata"; codeName = "06IRT.R"
# } else {
#   cat("Parametros de la función:\n")
#   cat("----> controlData: [Rdata] Class with parameters\n")
#   stop("**ERROR**  en los parametros")
# }

controlFile <- "controlData.Rdata"; codeName = "06IRT.R"

# # Cargando parametros de las pruebas
load(file.path(inPath, controlFile))
################################################################################
# # global definitions
################################################################################

setGeneric(name = "analIRT", def = function(object){standardGeneric("analIRT")})
setMethod("analIRT", "Prueba",
          analIRT <- function(object){

    idNoPKey <- c("O", "M")
	# # tipo de aplicacion 1 = Censal, 2 = Control, 3 = SobreMuestra,
	# # 4 = Especial, 5 = Adicional Censal, 6 = Adicional Control


	kApli <- c(2, 3, 4, 6)

	# # To conserved data from sample application if ESTUDIANTES then
	# # isTypeB = TRUE, if CLASE or ESCUELA then isTypeB = FALSE
	isTypeB <- FALSE

	# # to run the program for specific indexes
	isAllIndex <- TRUE

	# # vector with indexes, only if isAllIndex == FALSE
	indexWhich <- NULL

	# # models
	codesRasch           <- c("01", "02", "04")
	names(codesRasch)    <- c("PCM", "RSM", "MRD")
	codesNotRasch        <- c("03", "05")
	names(codesNotRasch) <- c("GRM", "2PL")

	# # to run Winsteps and Parscale, generalmente D = 1.7 en modelos IRT
	constDmodel <- 1.7

	# # deleted students with more than 80% of omission for the topic
	# # in the items that are not eliminated, this analysis is with the
	# # complete data set
	kOmissionThreshold  <- 1

	# # definition of cutoff values for item review
	kThresItemCorrDic    <- 0.2
	kThresItemCorrOrd    <- 0.2

	# # flag items with correlations less than 0.2 with the index
	kThresItemCor      <- 0.2

	# # flagCensal if TRUE get univariate from censal data
	# # this analysis is do it only with sample data
	flagCensal <- FALSE
	isCensal   <- is.null(flagCensal) | flagCensal

	# # categories will consider as No Response
	catToNA <- c('No Presentado', 'NR', 'Multimarca')

	# # cod for 'no eliminated' items
	kCodNElim <- '06'

	# # name of log file
	logFile <- file.path(logPath, "log.txt")

	versionOutput <- object@verSalida
  	verDataIn     <- object@verEntrada
	# # version of input response strings information and sample
	verDataIn <- object@verEntrada
	# # version of input dictionary
	verDictIn <- object@verEntrada

	################################################################################
	# # load data
	################################################################################
	# # output from 00CrearRdata.R
	load(object@pathDic)
  	load(object@pathRdata)

	################################################################################
	# # adjusting the DB
	################################################################################
	if(object@exam == "ACC"){
		if (isTypeB) {
		  # # ESTUDIANTES: conserved data from sample application
		  if (!is.data.frame(datBlock) & is.list(datBlock) & length(datBlock) == 1) {
		    datBlockControl <- list(subset(datBlock[[1]], tipoApli %in% kApli))
		    names(datBlockControl) <- names(datBlock)
		  } else {
		    datBlockControl <- lapply(datBlock, function(x)
		                          subset(x, x$tipoApli %in% kApli))
		  }
		} else {
			# # CLASE e INSTITUCION: for conserved data from sample application
			datBlockControl <- datBlock
		}

     	# # obtener los códigos de las pruebas leídas que corresponden a las
	  	# # pruebas que se desean analizar

		if(isTypeB){
		  varsKeep    <- c('codigo_prueba', 'codigo_forma', 'prueba')
		} else {
		  names(datBlockControl)[names(datBlockControl) == "noHoja"] <- "consLect"
		  varsKeep    <- c('codigo_prueba', 'prueba')
		}
	} else {
    	datBlockControl <- lapply(datBlock, function(x) {
        	                      aux <- data.frame(x$calBlock)
            	                  names(aux) <- gsub("X(\\d+)", "\\1", names(aux))
                	              return(aux)})
    	keyStrings  <- lapply(datBlock, function(x) attr(x, "keyString"))
    	varsKeep    <- c('codigo_prueba', 'prueba')
  	}

	################################################################################
	# # filtrando dictionaryList y datBlock si isAllIndex  == FALSE
	################################################################################
	if (isAllIndex) {
	  isNoElim      <- dictionaryList$variables[, 'elimina'] == kCodNElim
	  pruebasDesc   <- unique(dictionaryList$variables[isNoElim, varsKeep])
	  if (object@exam == "SABER359"){
	  	# # Quitando subloques de 3, 5, 9
	  	pruebasDesc <- subset(pruebasDesc, codigo_prueba %like% "pba\\w\\d{2}")
	  }
	} else {
      if(is.null(isDicIndex) | (!'isDicIndex' %in% object@param)){
    	stop("Cuando este apagado 'isAllIndex' Se debe especificar indexWhich = c('INDICEXXX1', ...)\n",
    		 "       Los indices que quiere correr.")
      }
	  cIndex   <- all(indexWhich %in% dictionaryList$variables[, "indice"])
	  if(isDicIndex) {
	    isIndexWhich <- dictionaryList$variables[, "indice"] %in% indexWhich
	    dictIndex    <- dictionaryList$variables[isIndexWhich, ]
	    isNoElim     <- dictIndex[, 'elimina'] == kCodNElim
	    pruebasDesc  <- unique(dictIndex[isNoElim, varsKeep])
	  } else {
	    stop("No todos los índices definidos (", indexWhich,") se encuentran
	         en el diccionario, verificar nombre")
	  }
	}

	# # pruebas para análisis
    if (object@exam == "ACC") {
	  exist <- pruebasDesc[, 'codigo_prueba'] %in% names(datBlock)
    } else {
      exist <- pruebasDesc[, 'codigo_prueba'] %in% gsub("\\.con", "", names(datBlock))
    }

	pruebasRead <- pruebasDesc[exist, 'codigo_prueba']
	pruebasRead <- sort(pruebasRead)

	for (kk in pruebasRead) {		
        nameNode      <- names(datBlockControl)[names(datBlockControl) %like% kk]
        # # keep items that aren't eliminated
        dictVarPrueba <- subset(dictionaryList$variables,
	                            codigo_prueba == kk & elimina == kCodNElim)
	  	if (object@exam == "SABER359"){
	    	gradoPba <- gsub(".+List_GR\\.(\\d).+", "\\1", object@pathDic)
           	nomPba   <- gsub(".+List_GR\\.(\\d)(pba|PBA)(\\w).+", "\\3", object@pathDic)
           	auxKeys        <- strsplit(keyStrings[[nameNode]], "")[[1]]
           	names(auxKeys) <- dictVarPrueba[, "id"]
           	if (length(dictVarPrueba[, "id"]) != length(auxKeys)) {
        	    stop("El string de respuesta no cuadra con el numero de ítems")
            }
	  	}

        if (! "indice" %in% names(dictVarPrueba)) {
           	dictVarPrueba[, "indice"] <- dictVarPrueba[, "codigo_prueba"]
        }

	  	dictVarPrueba <- subset(dictVarPrueba, indice != 'NI')

	  	if (object@exam == "ACC"){
	  		dictVarPrueba <- dictVarPrueba[order(dictVarPrueba[, 'orden']), ]
	  	}

	  	varId         <- dictVarPrueba[, 'id']

	 	# # Compare all items with data frame block
	  	isAllItemInDat <- all(varId %in% names(datBlockControl[[nameNode]]))

	  	if(!isAllItemInDat){
	    	cat("Warning!!!: Not all items in dictionary appear in data set!!!\n")
	  	}

	 	# # flag kOmissionThreshold
	 	isOmissDel <- kOmissionThreshold < 1 & kOmissionThreshold > 0
	 	# # recode to NA
	 	dataBlo     <- data.table(datBlockControl[[nameNode]][, varId])
	 	if (object@exam == "ACC") {
        	dataBlo[, ] <- lapply(dataBlo, RecodeToNA, catToNA)
            # # conserve rows with less than kOmissionThreshold NR data
            if (isOmissDel) {
	        	misRow  <- rowMeans(is.na(dataBlo))
	    	}
        } else {
            if (object@exam %in% c("SABERPRO", "SABER359")) {
              datBlockControl[[nameNode]] <- plyr::rename(datBlockControl[[nameNode]] ,
      		                                 c("SNP" = "noHoja"))
      	      # # porcentaje of omisions
	          if (isOmissDel) {
	            misRow  <- rowMeans(dataBlo == "")
	          }
        	}
	  }

	  # # conserve rows with less than kOmissionThreshold NR data
	  if (isOmissDel) {
	  	isKeep  <- misRow <= kOmissionThreshold
	  	dataBlo <- dataBlo[isKeep, ]
	  	personDataBlo <- datBlockControl[[nameNode]][isKeep , "noHoja"]
	  } else {
	  	personDataBlo <- datBlockControl[[nameNode]][ , "noHoja"]
	  }

	  if (nrow(dictVarPrueba) > 3) {
	    dataCor     <- dataBlo

	    if (object@exam == "ACC") {
	    	dataCor[, ] <- lapply(dataCor, as.numeric)
	    }

	    ###################################################
	    ### runIndices
	    ###################################################
	    if(!isAllIndex) {
	      isIndexWhich   <- dictVarPrueba[, "indice"]  %in% indexWhich
	      dictVarPrueba  <- dictVarPrueba[isIndexWhich, ]
	    }

	    if (object@exam != "ACC"){
	    	if (!"instr" %in% names(dictVarPrueba))
	    		dictVarPrueba[, 'instr'] <-  "No disponible"
			if (!"etiqu" %in% names(dictVarPrueba))
				dictVarPrueba[, 'etiqu'] <-  dictVarPrueba[, "indice"]
	    }
	    selectedModel <- unique(dictVarPrueba[, c("id", "indice", "instr", "etiqu")])
  	    # # ii = unique(selectedModel[, "indice"])[2]

	    for (ii in unique(selectedModel[, "indice"])) {
	      auxOutPath <- outPath
	      if (object@exam == "SABER359" & (!ii %like% "(pba|PBA)\\w\\d+")){
	      	outPath    <- file.path(outPath, paste(kk, ii, sep = "_"))
	      } else {
	      	outPath    <- file.path(outPath, ii)
	  	  }

	      if (!file.exists(outPath)) {
	        dir.create(outPath, recursive = TRUE)
	        dir.create(file.path(outPath, "salidas"), recursive = TRUE)
	        dir.create(file.path(outPath, "graficos"), recursive = TRUE)
	        dir.create(file.path(outPath, "corridas"), recursive = TRUE)
	      }

	      dicRasch     <- subset(dictVarPrueba, codMod %in% codesRasch)[, c("id", "indice")]
	      dicRasch     <- subset(dicRasch, indice == ii)
	      isIndexRasch <- nrow(dicRasch) > 0

              # # item that are part of the index
	      indexItems    <<- subset(selectedModel, indice == ii)[, "id"]
	      corBlock      <- dataCor[, indexItems, with = FALSE]

	      if (object@exam == "ACC"){
		    # # correlations item
		    itemCorrelations <- psych::alpha(corBlock, check.keys = isCheckKeys)$item.stats
		    itemCorrelations <- data.frame('item' = row.names(itemCorrelations),
		                                 'correlation' = itemCorrelations[, "r.drop"])
			isMissingHalf <- rowSums(!is.na(corBlock)) < max(length(indexItems) %/%  2, 3)

		    # # calibration criterion
		    indexreliability <- psych::alpha(corBlock)
		    alphaCoef        <- indexreliability$total[, "raw_alpha"]

		    nRatio <- 3000 / nrow(dataCor[!isMissingHalf, indexItems])
		    kThresItemChiHigh  <- 1 + (2/nRatio)
		    indexData <- paste("ind", ii, "_V", versionOutput, sep = "")
		  }

		  if (object@exam == "SABER359"){
		  	  orderSB       <- subset(dictVarPrueba, id %in% indexItems, select = c("id", "SUBBLOQUE"))
		  	  corBlock      <- corBlock[, orderSB[order(orderSB$SUBBLOQUE), "id"], with = FALSE]

		  }

                  if (object@exam %in% c("SABER359", "SABERPRO")) {
                    isMissingHalf <- rep(FALSE, nrow(corBlock))
                    if (ii %like% "(pba|PBA)\\w\\d+"){
		      indexData     <- paste(ii, "_V", versionOutput, sep = "")
		    } else {
		      indexData     <- paste("ind_", ii, "_", kk, "_V", versionOutput, sep = "")
		    }
                  }

	      # # Check the grouping in RSM blocks
		  rsmType             <- LETTERS[1:length(indexItems)]
		  codModels           <- subset(dictVarPrueba,
		                                indice == ii, select =  c("id", "codMod"))
		  rownames(codModels) <- codModels[, "id"]
		  codModels           <- codModels[indexItems, "codMod"]
		  names(codModels)    <- indexItems

		  # # Unique code model
		  if (length(unique(codModels)) > 1) {
		    stop("%%% Para este indice hay varios códigos de modelos")
		  }

		  # # Compare unique code of model
		  if (any(!codModels %in% dictionaryList$model[, "codMod"])) {
		    nError <- names(codModels)[!codModels %in% dictionaryList$model[, "codMod"]]
		    stop("%%% No existe el código del Modelo en el Diccionario, para
		           el/los ítem/s", paste(nError, collapse = " "))
		  }
		  # #
		  rsmType[codModels == codesRasch["RSM"]] <- 1

	      # # Data block to obtain results
	      resBlock <- dataCor[!isMissingHalf, indexItems, with = FALSE]

          # f_dowle2 = function(DT, value = " ") {
          #   for (i in names(DT))
          #     DT[get(i) == "", i := value, with=FALSE]
          # }
          # f_dowle2(resBlock)
	      if (isIndexRasch) {
	        # #     ##################
	        # #     #### OJO HASTA AQUI VA RASH
	        # #     ##################
	      }
              if(unique(codModels) %in% "07"){
                auxPath <- getwd()

                # #  Complete information of items
                itemFinal <- subset(dictVarPrueba, id %in% indexItems,
                                    select = c("id", "COMPONENTE", "COMPETENCIA", "SESION",
                                               "ORDEN", "EQUATING", "SUBBLOQUE"))
                itemFinal <- apply(itemFinal[match(indexItems, itemFinal$id), ],
                                   1, paste, collapse = "")

                # # Create .con and .dat file
                # resBloc debe tener las respuestas NO codificadas (cambiar linea 232)
                # RunWinsteps(responses = resBlock,
                #             keysItems = auxKeys[names(resBlock)],
                #             runName = indexData,
                #             outPath = file.path("..", 'salidas'),
                #             runPath = file.path(outPath, 'corridas'),
                #             personIds = personDataBlo[!isMissingHalf], itemIds = itemFinal,
                #             tables = c(1, 3, 14), quotes = "Y", sep = "\t",
                #             verbose = TRUE, runProgram = FALSE)

                # # Run Bilog from Winsteps
                #RunFromWN(runPath = file.path(outPath, 'corridas'),
                #          scriptPath = '..\\..\\..\\..\\Src\\PERL\\')

                # # Create .blm and .dat file
                personDataBlo <- personDataBlo[!isMissingHalf]
                indexData <- gsub("(pba|PBA)","", indexData)
                
                if(object@exam == "SABERPRO"){
                  itemFinal <- substr(itemFinal, 1, 6)
                }
                
                RunBilog(responseMatrix = resBlock,
                         runName = indexData,  srcPath = auxPath,
                         outPath = file.path("..", 'salidas'),
                         personIds = personDataBlo,
                         itemIds = itemFinal, binPath = binPath,
                         runPath = file.path(outPath, 'corridas'),
                         verbose = TRUE, runProgram = TRUE,
                         commentFile = indexData, NPArm = 3)


            # Reading results of parameters model
	      	itemDiffFile   <- paste("salidas/", indexData, ".PAR", sep = "")
	      	itemParameters <- try(ReadBlParFile(itemDiffFile, outPath))

	        # # Reading TCT results
	      	itemTCTFile <- paste("salidas/", indexData, ".TCT", sep = "")
	      	tctParam    <- try(ReadBlTCTFile(itemTCTFile, outPath))

	      	# # Resultados estimación de habilidades
	        personAbilFile  <- paste("salidas/", toupper(indexData), ".SCO", sep = "")
	        outFileAbili    <- file.path(outPath, "salidas",
	                                     paste0("personAbilities_V",
	                                           versionOutput, ".RData"))
	        if (!file.exists(outFileAbili)) {
	        	personAbilities <- try(ReadBlScoFile(personAbilFile, outPath,
	        		                   lengthIds = nchar(personDataBlo[1]), 
	        		                   flagGr = object@exam != "SABERPRO"))
	        	save(personAbilities, file = outFileAbili)
	        } else {
	        	# personAbilities[, "iSubject"]<- 1:nrow(personAbilities)
	        	load(outFileAbili)
	        }

	      	if (class(itemParameters) == "try-error" |
	      		class(tctParam) == "try-error" |
	      		class(personAbilities) == "try-error") {
                stop("Error en Bilog no se pudo leer alguno",
                	 "de estos archivos (.PAR, .TCT, .SCO)")
	      	}

	 		# # Responses Curve
	      	resBlockOri <- subset(datBlock[[nameNode]]$oriBlock,
	      		                  SNP %in% personDataBlo,
	      		                  select = indexItems)
			resBlockOri[, "iSubject"] <- 1:nrow(resBlockOri)
            if (object@exam == "SABER359") {
            	dirPlotOPpng <- paste0("graficos/plotOP-", gradoPba,
                                	nomPba, "-", indexData, ".png")                                	
            } else {
            	dirPlotOPpng <- paste0("graficos/plotOP-", indexData, ".png")                                	
            }
            
            dirPlotOPpng <- file.path(outPath, dirPlotOPpng)
	        keyData      <- subset(dictVarPrueba, id %in% indexItems,
	        	                   select = c("id", "keyItem"))
	        dirCatFre <- file.path(outPath, "salidas",
	        	                   paste0("catFreq_V",
	                                      versionOutput, ".RData"))
	        listOP <- responseCurve(resBlockOri, personAbilities,
	                                methodBreaks = "Sturges",
	                                keyData = keyData,
	                                dirPlot = dirPlotOPpng, plotName =
                                    "Opciones Item", dirCatFreq = dirCatFre)
	        load(dirCatFre)  # This load tablaRep Constructed by responseCurve

	      	# # ICC curve
      		resBlock[,"consLect"]  <- personDataBlo[!isMissingHalf]
	        resBlock[, "iSubject"] <- 1:nrow(resBlock)

			if (object@exam == "SABER359") {
            	dirPlotICCpng <- paste0("graficos/plotICC-", gradoPba,
                                	nomPba, "-", indexData, ".png")                                	
            } else {
            	dirPlotICCpng <- paste0("graficos/plotICC-", indexData, ".png")                                	
            }
	        dirPlotICCpng <- file.path(outPath, dirPlotICCpng)
	        listICC <- plotICCB(itemParameters, resBlock, personAbilities,
	                   scaleD = constDmodel, flagGrSep = TRUE,
	                   methodBreaks = "Sturges", nameIndice = ii,
	                   dirPlot = dirPlotICCpng, plotName =
	                   paste("ICC para", object@nomPrueba), prueba = kk, 
	                   dirSalida = outPath)

	      	# # Items - Person Curve
	      	if (object@exam == "SABER359") {
            	dirPerItem <- paste0("graficos/personItem-", gradoPba,
                                	nomPba, "-", indexData, ".png")                                	
            } else {
            	dirPerItem <- paste0("graficos/personItem-", indexData, ".png")                                	
            }
	      	dirPerItem <- file.path(outPath, dirPerItem)
	      	pathGraph  <- object@nomPrueba
	      	itHaMap    <- WrightMapICFES(itemParameters, personAbilities,
                                         "ABILITY", "dif", file = dirPerItem,
                                         Title = gsub("(\\s+)?SABER 3,\\s?5 y 9 ", "", pathGraph))


	        # # Information of block
			if (object@exam == "SABER359") {
	  		  infoBloque <- data.table(dictVarPrueba)
	  		  infoBloque <- infoBloque[id %in% indexItems, ]
			  infoBloque <- infoBloque[match(indexItems, infoBloque$id),
	  		                           list(codigo_prueba, COMPONENTE,
	                           	            COMPETENCIA, keyItem, 'item_cod' = id,
	  		                           	    'item' = paste0(SUBBLOQUE, sprintf("%.2d", 1:length(id)))),
	  		                                by = "SUBBLOQUE"]
	        } else {
	          infoBloque <- data.table(dictVarPrueba)
	  		  infoBloque <- infoBloque[id %in% indexItems, ]
			  infoBloque <- infoBloque[match(indexItems, infoBloque$id),
	  		                           list('item' = paste0("ITEM", sprintf("%.4d", 1:length(id))),
                                         	'item_cod' = id, codigo_prueba, keyItem)]
	        }

			# # Codigos de los subloques
			itemParameters <- merge(itemParameters, infoBloque, by = "item", all.x = TRUE)
			setnames(itemParameters, c("item", "item_cod"), c("item_blq", "item"))
			selecColTCT    <- names(tctParam)
			tctParam 	   <- merge(tctParam, infoBloque, by = "item", all.x = TRUE)
			setnames(tctParam, c("item", "item_cod"), c("item_blq", "item"))
			tctParam       <- tctParam[, selecColTCT]

			# # Construct Flag Proportions and flag of habilities
			tablaFlags  <- merge(data.table(itemParameters)[, list(item, keyItem)],
				                 tablaRep, by = "item", all.y = TRUE)
			tablaFlags[keyItem == categoria, keyAbility := mAbility]
			tablaFlags[, keyAbility := na.omit(keyAbility)[1], by = "item"]
            tablaFlags  <- tablaFlags[!categoria %in% idNoPKey,
                                      list('FLAGPROP' = ifelse(min(prop) < 0.1 | max(prop) >= 0.9, 1, 0),
            	                           'FLAGKEY2'  = ifelse(sum(mAbility > keyAbility), 1, 0)), by = "item"]

			# # Construct Data Consolidation
			ldirOP   <- data.table('item' = names(listOP), 'dir_OP' = sapply(listOP, function(x) x$dir))
			ldirICC  <- data.table('item' = names(listICC), 'dir_ICC' = sapply(listICC, function(x) x$dir))
			tablaRep <- melt(tablaRep, id = 1:2, measure = 3:4)
			tablaRep <- dcast.data.table(tablaRep, item ~ categoria + variable, fun = sum,
				                         value.var = c("value"))
			tablaFin <- merge(merge(ldirOP, ldirICC, by = "item"), tablaRep, by = "item")
			tablaFin <- merge(tablaFin, itemParameters, by = "item")
			tablaFin <- merge(tablaFin, tctParam, by = "item")
			tablaFin <- merge(tablaFin, tablaFlags, by = "item")

			# # Fill other columns in the report
            colFix <- c("CORRELACION", "PCT", "disc", "dif", "azar","INFIT", "OUTFIT", "BISERIAL", "item_blq", "TRIED", "SUBBLOQUE",
            			"COMPONENTE", "COMPETENCIA")
            colFix <- colFix[!colFix %in% names(tablaFin)]
            for(col in colFix){
            	tablaFin <- cbind(tablaFin, 'newCol' = "No aplica")
            	setnames(tablaFin, 'newCol', col)
            }

		    # # Other Flags
            tablaFin[, indPos := ifelse(TRIED <= 200, 1, ifelse(TRIED > 100000, 4, ifelse(TRIED > 500, 3, 2))), by = item]
            tablaFin[, minOutms := c(0.7, 0.75, 0.8, 0.9)[indPos]]
            tablaFin[, maxOutms := c(1.3, 1.25, 1.2, 1.1)[indPos]]

            # # Modifica William 
			tablaFin <- cbind(tablaFin, tablaFin[, list(
				                      'codMOD' = unique(codModels),
 				                      'FLAGMEAN' = ifelse((PCT >90) | (PCT < 10), 1, 0),
			                          'FLAGCORR' = ifelse(CORRELACION < 0.1, 1, 0),
			                          'FLAGA'    = ifelse(disc < 0.5 , 1, 0),
			                          'FLAGB'    = ifelse(dif > 3, 1, 0),
			                          'FLAGBISE' = ifelse(BISERIAL < 0.1, 1, 0), 
			                          'FLAGINFIT' = ifelse((INFIT < minOutms[indPos]) | (INFIT > maxOutms[indPos]), 1, 0),
			                          'FLAGOUTFIT' = ifelse((OUTFIT < minOutms[indPos]) | (OUTFIT > maxOutms[indPos]), 1, 0), 
			                          'FLAGKEY1' = 0, 'FLAGKEY3' = 0,
			                          'FLAGDIFDIS' = ifelse(dif < -3 & disc > 0.5, 1, 0),
			                          'FLAGAZAR'  = ifelse(azar > 0.25 | eeazar > 0.15, 1, 0))])

			save(tablaFin, file = file.path(outPath, "datosReport.Rdata"))
			return(list('datosReport' = tablaFin, 'mapa' = itHaMap,
				        'listOP' = listOP, 'listICC' = listICC))
          }

# 	      if (unique(codModels) %in% "05") {
# 	   	    ##########################################################################
# 	   	    # # Corrida para la calibración modelo 2PL SABER 3, 5 y 9
# 	        ##########################################################################
#             auxPath <- getwd()
# 
#             # #  Complete information of items
#             itemFinal <- subset(dictVarPrueba, id %in% indexItems,
#                                 select = c("id", "COMPONENTE", "COMPETENCIA", "SESION",
#                    	            "ORDEN", "EQUATING", "SUBBLOQUE"))
#             itemFinal <- apply(itemFinal[match(indexItems, itemFinal$id), ],
#             	               1, paste, collapse = "")
# 
#             # # Create .con and .dat file
#             # resBloc debe tener las respuestas NO codificadas (cambiar linea 232)
# 	        # RunWinsteps(responses = resBlock,
#             #             keysItems = auxKeys[names(resBlock)],
#             #             runName = indexData,
#             #             outPath = file.path("..", 'salidas'),
#             #             runPath = file.path(outPath, 'corridas'),
#             #             personIds = personDataBlo[!isMissingHalf], itemIds = itemFinal,
#             #             tables = c(1, 3, 14), quotes = "Y", sep = "\t",
#             #             verbose = TRUE, runProgram = FALSE)
# 
# 	        # # Run Bilog from Winsteps
# 	        #RunFromWN(runPath = file.path(outPath, 'corridas'),
#             #          scriptPath = '..\\..\\..\\..\\Src\\PERL\\')
# 
# 	        # # Create .blm and .dat file
# 	        personDataBlo <- personDataBlo[!isMissingHalf]
# 	        RunBilog(responseMatrix = resBlock,
#                      runName = indexData,  srcPath = auxPath,
#                      outPath = file.path("..", 'salidas'),
#                      personIds = personDataBlo,
#                      itemIds = itemFinal, binPath = binPath,
#                      runPath = file.path(outPath, 'corridas'),
#                      verbose = TRUE, runProgram = TRUE,
#                      commentFile = paste0(gradoPba, nomPba, "-", ii))
# 
# 	        # # Reading results of parameters model
# 	      	itemDiffFile   <- paste("salidas/", indexData, ".PAR", sep = "")
# 	      	itemParameters <- try(ReadBlParFile(itemDiffFile, outPath))
# 
# 	        # # Reading TCT results
# 	      	itemTCTFile <- paste("salidas/", indexData, ".TCT", sep = "")
# 	      	tctParam    <- try(ReadBlTCTFile(itemTCTFile, outPath))
# 
# 	      	# # Resultados estimación de habilidades
# 	        personAbilFile  <- paste("salidas/", toupper(indexData),
# 	                               ".SCO", sep = "")
# 	        outFileAbili    <- file.path(outPath, "salidas",
# 	                                     paste0("personAbilities_V",
# 	                                           versionOutput, ".RData"))
# 	        if (!file.exists(outFileAbili)) {
# 	        	personAbilities <- try(ReadBlScoFile(personAbilFile, outPath,
# 	        		                   lengthIds = nchar(personDataBlo[1])))
# 	        	save(personAbilities, file = outFileAbili)
# 	        } else {
# 	        	# personAbilities[, "iSubject"]<- 1:nrow(personAbilities)
# 	        	load(outFileAbili)
# 	        }
# 
# 	      	if (class(itemParameters) == "try-error" |
# 	      		class(tctParam) == "try-error" |
# 	      		class(personAbilities) == "try-error") {
#                 stop("Error en Bilog no se pudo leer alguno",
#                 	 "de estos archivos (.PAR, .TCT, .SCO)")
# 	      	}
# 
# 	 		# # Responses Curve
# 	      	resBlockOri <- subset(datBlock[[nameNode]]$oriBlock,
# 	      		                  SNP %in% personDataBlo,
# 	      		                  select = indexItems)
# 			resBlockOri[, "iSubject"] <- 1:nrow(resBlockOri)
#             dirPlotOPpng <- paste0("graficos/plotOP-", gradoPba,
#                                 	nomPba, "-", indexData, ".png")
#             dirPlotOPpng <- file.path(outPath, dirPlotOPpng)
# 	        keyData      <- subset(dictVarPrueba, id %in% indexItems,
# 	        	                   select = c("id", "keyItem"))
# 	        dirCatFre <- file.path(outPath, "salidas",
# 	        	                   paste0("catFreq_V",
# 	                                      versionOutput, ".RData"))
# 	        listOP <- responseCurve(resBlockOri, personAbilities,
# 	                                methodBreaks = "Sturges",
# 	                                keyData = keyData,
# 	                                dirPlot = dirPlotOPpng, plotName =
#                                     "Opciones Item", dirCatFreq = dirCatFre)
# 	        load(dirCatFre)  # This load tablaRep Constructed by responseCurve
# 
# 	      	# # ICC curve
#       		resBlock[,"consLect"]  <- personDataBlo[!isMissingHalf]
# 	        resBlock[, "iSubject"] <- 1:nrow(resBlock)
# 
# 	        dirPlotICCpng <- paste0("graficos/plotICC-", gradoPba,
# 	                            	nomPba, "-", indexData, ".png")
# 	        dirPlotICCpng <- file.path(outPath, dirPlotICCpng)
# 	        listICC <- plotICCB(itemParameters, resBlock, personAbilities,
# 	                   scaleD = constDmodel, flagGrSep = TRUE,
# 	                   methodBreaks = "Sturges", nameIndice = ii,
# 	                   dirPlot = dirPlotICCpng, plotName =
# 	                   paste("ICC para", object@nomPrueba))
# 
# 	      	# # Items - Person Curve
# 	      	dirPerItem <- paste0("graficos/personItem-", gradoPba,
# 	                             nomPba, "-", indexData, ".png")
# 	      	dirPerItem <- file.path(outPath, dirPerItem)
# 	      	pathGraph  <- object@nomPrueba
# 	      	itHaMap    <- WrightMapICFES(itemParameters, personAbilities,
#                                          "ABILITY", "dif", file = dirPerItem,
#                                          Title = gsub("(\\s+)?SABER 3,\\s?5 y 9 ", "", pathGraph))
# 
# 
# 	        # # Information of block
# 			if(object@exam == "SABER359") {
# 	  		  infoBloque <- data.table(dictVarPrueba)
# 	  		  infoBloque <- infoBloque[id %in% indexItems, ]
# 			  infoBloque <- infoBloque[match(indexItems, infoBloque$id),
# 	  		                           list(codigo_prueba, COMPONENTE,
# 	                           	            COMPETENCIA, keyItem, 'item_cod' = id,
# 	  		                           	    'item' = paste0(SUBBLOQUE, sprintf("%.2d", 1:length(id)))),
# 	  		                                by = "SUBBLOQUE"
# 	                                        ]
# 	        }
# 
# 			# # Codigos de los subloques
# 			itemParameters <- merge(itemParameters, infoBloque, by = "item", all.x = TRUE)
# 			setnames(itemParameters, c("item", "item_cod"), c("item_blq", "item"))
# 			selecColTCT    <- names(tctParam)
# 			tctParam 	   <- merge(tctParam, infoBloque, by = "item", all.x = TRUE)
# 			setnames(tctParam, c("item", "item_cod"), c("item_blq", "item"))
# 			tctParam       <- tctParam[, selecColTCT]
# 
# 			# # Construct Flag Proportions and flag of habilities
# 			tablaFlags  <- merge(data.table(itemParameters)[, list(item, keyItem)],
# 				                 tablaRep, by = "item", all.y = TRUE)
# 			tablaFlags[keyItem == categoria, keyAbility := mAbility]
# 			tablaFlags[, keyAbility := na.omit(keyAbility)[1], by = "item"]
#             tablaFlags  <- tablaFlags[!categoria %in% idNoPKey,
#                                       list('FLAGPROP' = ifelse(min(prop) < 0.1 | max(prop) >= 0.9, 1, 0),
#             	                           'FLAGKEY2'  = ifelse(sum(mAbility > keyAbility), 1, 0)), by = "item"]
# 
# 			# # Construct Data Consolidation
# 			ldirOP   <- data.table('item' = names(listOP), 'dir_OP' = sapply(listOP, function(x) x$dir))
# 			ldirICC  <- data.table('item' = names(listICC), 'dir_ICC' = sapply(listICC, function(x) x$dir))
# 			tablaRep <- melt(tablaRep, id = 1:2, measure = 3:4)
# 			tablaRep <- dcast.data.table(tablaRep, item ~ categoria + variable, fun = sum,
# 				                         value.var = c("value"))
# 			tablaFin <- merge(merge(ldirOP, ldirICC, by = "item"), tablaRep, by = "item")
# 			tablaFin <- merge(tablaFin, itemParameters, by = "item")
# 			tablaFin <- merge(tablaFin, tctParam, by = "item")
# 			tablaFin <- merge(tablaFin, tablaFlags, by = "item")
# 
# 			# # Fill other columns in the report
#             colFix <- c("CORRELACION", "PCT", "disc", "dif", "INFIT", "OUTFIT", "BISERIAL", "item_blq", "TRIED")
#             colFix <- colFix[!colFix %in% names(tablaFin)]
#             for(col in colFix){
#             	tablaFin <- cbind(tablaFin, 'newCol' = NA)
#             	setnames(tablaFin, 'newCol', col)
#             }
# 
# 		    # # Other Flags
#             tablaFin[, indPos := ifelse(TRIED <= 200, 1, ifelse(TRIED > 100000, 4, ifelse(TRIED > 500, 3, 2))), by = item]
#             tablaFin[, minOutms := c(0.7, 0.75, 0.8, 0.9)[indPos]]
#             tablaFin[, maxOutms := c(1.3, 1.25, 1.2, 1.1)[indPos]]
# 
# 			tablaFin <- cbind(tablaFin, tablaFin[, list('FLAGMEAN' = ifelse((PCT >90) | (PCT < 10), 1, 0),
# 			                          'FLAGCORR' = ifelse(CORRELACION < 0.1, 1, 0),
# 			                          'FLAGA'    = ifelse(disc < 0.5, 1, 0),
# 			                          'FLAGB'    = ifelse(dif > 3, 1, 0),
# 			                          'FLAGBISE' = ifelse(BISERIAL < 0.1, 1, 0),
# 			                          'FLAGINFIT' = ifelse((INFIT < minOutms[indPos]) | (INFIT > maxOutms[indPos]), 1, 0),
# 			                          'FLAGOUTFIT' = ifelse((OUTFIT < minOutms[indPos]) | (OUTFIT > maxOutms[indPos]), 1, 0),
# 			                          'FLAGKEY1' = 0, 'FLAGKEY3' = 0)])
# 
# 
# 			save(tablaFin, file = file.path(outPath, "datosReport.Rdata"))
# 			return(list('datosReport' = tablaFin, 'mapa' = itHaMap,
# 				        'listOP' = listOP, 'listICC' = listICC, 'mapaDir' = ))
#           }
# 
# 	      if (!isReporte & unique(codModels) %in% "05") {
# 	    	  outFile <- file.path(outPath,
# 	        	           paste('06IRT_', saber, '_', indexData, ".xlsx", sep = ''))
# 		  }
# 
#   	      if (unique(codModels) %in% "03") {
# 
# 	      auxPath <- getwd()
# 	      ################################################################################
# 	      # Correr PARSCALE
# 	      ################################################################################
# 	      invisible(RunParscale(responseMatrix = resBlock,
# 	                        runName = indexData,
# 	                        outPath = file.path("..", 'salidas'),
# 	                        runPath = file.path(outPath, 'corridas'),
# 	                        score = TRUE, rsm = rsmType,
# 	                        group = NULL, weights = NULL,
# 	                        rasch = rep(FALSE, length(table(rsmType))),
# 	                        itemIds = indexItems, kD = constDmodel,
# 	                        personIds = personDataBlo[!isMissingHalf],
# 	                        binPath = binPath,
# 	                        verbose = FALSE,
# 	                        skipVector = rep(FALSE, length(table(rsmType))),
# 	                        srcPath = auxPath,
# 	                        commentFile = paste("For the test", gsub(" ", "_", kk),
# 	                                "and Index", ii)))
# 
# 	      # # Se verifica si el archivo PH2 tiene un error
# 	      # # por estimabilidad de los parámetros de
# 	      # # localización en PCM GRM
# 	      itemPH2File   <- paste("corridas/", indexData, ".PH2", sep = "")
# 	      testPH2OK     <- testPH2FilePS(fileName = itemPH2File,
# 	                                     filePath = outPath, length(indexItems))
# 
# 	      if(testPH2OK$error) isErrorItemFit <- TRUE
# 	      if(!testPH2OK$error) {
# 
# 	      # # Resultados estimación de parámetros
# 	      itemDiffFile   <- paste("salidas/", indexData, ".pslip", sep = "")
# 	      itemParameters <- try(ReadPsParFile(itemDiffFile, outPath))
# 
# 	      rm(itemDiffFile)
# 
# 	      # # Filtro cuando no se puede calcular paramétros - Error matriz
# 	      # # singular en la corrida
# 	      if (class(itemParameters) != "try-error") {
# 
# 	        itemParameters[, "itemName"] <- indexItems
# 
# 	        stepNames      <- grep("step", names(itemParameters), value = TRUE)
# 	        itemThresholds <- itemParameters[, c("itemName", stepNames)]
# 
# 	        itemThresholds <- reshape(itemThresholds, direction = "long",
# 	                                varying = stepNames)
# 	        names(itemThresholds) <- c("itemName", "category", "threshold", "order")
# 
# 	        # # Simulate Winsteps Thresholds output
# 
# 	        category1      <- itemThresholds[, "category"] == 1
# 	        itemThresZero  <- itemThresholds[category1, ]
# 	        itemThresZero[, c("category", "threshold")] <- 0
# 	        itemThresholds <- rbind(itemThresZero, itemThresholds)
# 
# 	        # # Resultados estimación de habilidades
# 	        personAbilFile  <- paste("salidas/", toupper(indexData),
# 	                               ".PSLSC", sep = "")
# 	        personAbilities <- ReadPsScoFile(personAbilFile, outPath)
# 
# 	        rm(personAbilFile)
# 
# 	        abilities <- file.path(outPath, paste("Abilities", ii, ".txt", sep = ""))
# 	        personAbilities[, "FA"] <- kk
# 	        personAbilities[, "indice"] <- ii
# 
# 	        recodSitio   <- sprintf("%.6d", as.numeric(personAbilities[,
# 	                                                 "idSubject"]))
# 	        personAbilities[, "idSubject"] <- recodSitio
# 
# 	        # # guardando archivo con las habilidades estimadas (calificación)
# 	        outFileAbili <- file.path(outPath, "salidas",
# 	                                  paste("personAbilities_V",
# 	                                        versionOutput, ".RData", sep =
# 	                                        ""))
# 	        save(personAbilities, file = outFileAbili)
# 
# 
# 	        names(itemCorrelations)[1] <- "itemName"
# 
# 	        isErrorItemFit        <- FALSE
# 	        itemFitFile           <- paste("corridas/", indexData, ".PH2", sep = "")
# 	        itemFit               <- try(ReadPsChiFitFile(fileName = itemFitFile,
# 	                                                    filePath = outPath))
# 	        rm(itemFitFile)
# 
# 	        if (class(itemFit) != "try-error") {
# 
# 	          itemFit[, "itemName"] <- indexItems
# 	          itemFit[, "df"]       <- as.integer(itemFit[, "df"])
# 
# 	          # # Create output table
# 	          isDicItems <- which(unclass(by(itemThresholds[, "category"],
# 	                                       itemThresholds[, "order"],
# 	                                       function (x) length(table(x)))) == 2)
# 
# 	          isDicItems <- itemThresholds[,"order"] %in% isDicItems
# 
# 	          kThresholdsItemCorr <- rep(kThresItemCorrOrd, length(isDicItems))
# 	          kThresholdsItemCorr[isDicItems] <- kThresItemCorrDic
# 	          kThresholdsItemCorr <- data.frame(order = itemThresholds[, "order"],
# 	                                            kThreshold = kThresholdsItemCorr)
# 	          kThresholdsItemCorr <- unique(kThresholdsItemCorr)
# 	          kThresholdsItemCorr <- merge(kThresholdsItemCorr, itemParameters)
# 	          kThresholdsItemCorr <- kThresholdsItemCorr[, c("itemName", "kThreshold")]
# 	          kThresholdsItemCorr <- unique(kThresholdsItemCorr)
# 
# 	          if (any(!isDicItems)) {
# 	            itemThresholds <- itemThresholds[!isDicItems, ]
# 	          }
# 	          itemThresholds[, "category"] <- as.integer(itemThresholds[, "category"])
# 
# 	          newAlpha <- data.frame(itemName = rownames(indexreliability$alpha.drop),
# 	                               indexreliability$alpha.drop)
# 	          indexreliability$alpha.drop <- newAlpha
# 	          rm(newAlpha)
# 
# 	          # # Tabla para exportación con estimación de parámetros
# 
# 	          if (any(!isDicItems)) {
# 	            outItems   <- merge(merge(merge(merge(itemFit, itemCorrelations),
# 	                        indexreliability$alpha.drop[, c("itemName", "raw_alpha")]),
# 	                        itemParameters, by = "itemName"),
# 	                                itemThresholds,
# 	                        all.x = TRUE)
# 
# 	            orderRows  <- order(outItems[, "order"], outItems[, "category"])
# 	            outItems   <- outItems[orderRows, ]
# 	            nSteps   <- max(outItems[, "category"])
# 	            stepSEStep    <- paste("seStep", rep(1:nSteps),
# 	                                      sep=".")
# 
# 	            outColumns <- c("itemName", "category", "location", "discrimination",
# 	                          "correlation", "raw_alpha", "chiSquare", "df",
# 	                          "seLocation", stepSEStep, "seDiscrimination")
# 
# 	            stepRows <- which(outItems[, "category"] > 0)
# 	            outItems[stepRows, "location"]  <-  outItems[stepRows, "threshold"]
# 
# 	            outItems   <- outItems[, outColumns]
# 
# 	            if (isReporte) {
# 	              outItems[stepRows,  c("itemName", "discrimination", "correlation",
# 	                                "raw_alpha", "chiSquare", "df")] <- NA
# 	              outItems[!stepRows, "category"] <- NA
# 	            }
# 	            else {
# 	               outItems <- reshape(outItems,
# 	                                   idvar = c("itemName", "discrimination",
# 	                                             "correlation", "raw_alpha", "chiSquare",
# 	                                             "df", "seLocation", stepSEStep, "seDiscrimination"),
# 	                                   timevar = "category", direction = "wide")
# 	            }
# 	          } else {
# 	            outItems   <- merge(merge(merge(itemFit, itemCorrelations),
# 	                        indexreliability$alpha.drop[, c("itemName", "raw_alpha")]),
# 	                        itemParameters, by = "itemName")
# 	            outColumns <- c("itemName", "location", "discrimination", "correlation",
# 	                          "raw_alpha", "chiSquare", "df", "seLocation",
# 	                          "seDiscrimination")
# 	            outItems   <- outItems[, outColumns]
# 	            nSteps    <- 0  ## modificado Fabio
# 	            if(exists("stepThreshold")) rm(stepThreshold)
# 	          }
# 
# 	          relChiSq <- outItems[, "chiSquare"] / outItems[, "df"]
# 
# 	          reviewItems <- rep(NA, length = nrow(outItems))
# 
# 	          for (jj in seq(nrow(outItems))) {
# 
# 	            if (!is.na(outItems[jj, "correlation"])) {
# 	              itemjj <- which(kThresholdsItemCorr[, "itemName"] ==
# 	                            outItems[jj, "itemName"])
# 	              kThresItemCorr <- kThresholdsItemCorr[itemjj, "kThreshold"]
# 
# 	              if (outItems[jj, "correlation"] < kThresItemCorr) {
# 	                reviewItems[jj] <- "*"
# 	              }
# 
# 	              if (outItems[jj, "raw_alpha"] > (alphaCoef + 0.05)) {
# 	                reviewItems[jj] <- "*"
# 	              }
# 	            }
# 
# 	            if (!is.na(relChiSq[jj])) {
# 
# 	              if (relChiSq[jj] > 3 | relChiSq[jj] < kThresItemChiHigh) {
# 	                reviewItems[jj] <- "*"
# 	              }
# 
# 	              if (relChiSq[jj] >  (kThresItemChiHigh - 1)) {
# 	                reviewItems[jj] <- "*"
# 	              }
# 
# 	          # #               if (relChiSq[jj] < kThresItemChiLow) {
# 	          # #                 reviewItems[jj] <- "*"
# 	          # #               }
# 	            }
# 	          }
# 
# 	          outItems <- data.frame(outItems, relChiSq)
# 
# 	          if (any(!is.na(reviewItems))) {
# 	            outItems <- data.frame(outItems, Revisar = reviewItems)
# 	          }
# 
# 	          # # Begin items table
# 
# 	          # # si es menor que 12 son índices con todos los ítems dicotómicos
# 	          if (ncol(outItems) < 12) {
# 
# 	            # # para índices con todos los ítems dicotómicos
# 	            outItems[, "Threshold"] <- outItems[ , "location"]
# 
# 	            # # sin nada que revisar
# 	            orderOut <- c("itemName", "location", "discrimination",
# 	                          "correlation", "raw_alpha", "chiSquare", "df",
# 	                          "relChiSq", "seLocation", "seDiscrimination",
# 	                          "Threshold")
# 	            outNames  <- c("Ítem", "Dificultad", "Discriminación",
# 	                               "Correlación", "alpha_{-i}", "chi^2",
# 	                               "gl", "chi^2/gl", "EEDificultad",
# 	                               "EEDiscriminación", "Threshold")
# 
# 	            if ("Revisar" %in% names(outItems)) {
# 	             # # con algo que revisar
# 	             orderOut <- c(orderOut, "Revisar")
# 	             outNames <- c(outNames, "Revisar")
# 	            }
# 
# 	            outItems <-  outItems[, orderOut]
# 	            names(outItems) <- outNames
# 	          } else {
# 	              # # índices con todos los ítems no dicotómicos
# 	                stepLocation    <- paste("location", rep(0:nSteps),
# 	                                      sep=".")
# 	                stepDificultad  <- c("Dificultad",
# 	                                     paste("DificulPaso", rep(1:nSteps),
# 	                                      sep=""))
# 	                stepEE <- c(paste("EE", stepDificultad, sep = ""),
# 	                                "EEDiscriminación")
# 
# 	                # # generación de los umbrales para anexar a tabla outItems
# 	                  for(tt in c(1:nSteps)) {
# 	                    threshold <- paste("location.", tt, sep = "")
# 	                    nameThreshold <- paste("Threshold", tt, sep = "")
# 	                    outItems[, nameThreshold] <- outItems[, "location.0"] -
# 	                                          outItems[, threshold]
# 
# 	                  }
# 	                stepThreshold  <- paste("Threshold", rep(1:nSteps),
# 	                                      sep="")
# 
# 	                # # sin nada que revisar
# 	            orderOut <- c("itemName", stepLocation,
# 	                              "discrimination",
# 	                              "correlation",
# 	                              "raw_alpha", "chiSquare", "df",
# 	                              "relChiSq",
# 	                              "seLocation", stepSEStep,
# 	                              "seDiscrimination", stepThreshold)
# 	            outNames <- c("Ítem", stepDificultad,
# 	                                       "Discriminación",
# 	                                 "Correlación", "alpha_{-i}", "chi^2",
# 	                                 "gl", "chi^2/gl", stepEE, stepThreshold)
# 
# 	            if ("Revisar" %in% names(outItems)) {
# 	             # # con algo que revisar
# 	             orderOut   <- c(orderOut, "Revisar")
# 	             outNames <- c(outNames, "Revisar")
# 	            }
# 
# 	            outItems <-  outItems[, orderOut]
# 	            names(outItems) <- outNames
# 	          }
# 
# 	          if (isReporte) {
# 	            tabFile <- file.path(outPath,
# 	                       paste('06IRT_', saber, '_pba', kk, indexData,
# 	                             ".txt", sep = ''))
# 	            write.table(outItems, file = tabFile, row.names = FALSE, sep = "\t")
# 	          } else {
# 	            outItems <- outItems[ , !(names(outItems) %in% c("Revisar"))]
# 	          }
# 
# 	          # # Gráfica CMC
# 
# 	          # # Tabla confiabilidades sin el ítem (CMC)
# 	          if (length(indexItems) > 3) {
# 	            ConfTot <- CMC::alpha.curve(corBlock[complete.cases(corBlock),])
# 	          } else {
# 	            # # Se debe armar la tabla  ConfTot debido a que CMC::alpha.curve
# 	            # # presenta error con solo 3 ítems
# 	            maxAlphaRem <- max(indexreliability$alpha.drop$raw_alpha)
# 	            isMaxAlphaRem <- indexreliability$alpha.drop$raw_alpha == maxAlphaRem
# 	            itemRem <- as.character(indexreliability$alpha.drop$item[isMaxAlphaRem])
# 
# 	            ConfTot <- data.frame( N.Item = c(length(indexItems) - 1,
# 	                                              length(indexItems)),
# 	                                   Alpha.Max = c(maxAlphaRem, alphaCoef),
# 	                                   Removed.Item = c(itemRem, '--'))
# 	          }
# 
# 	          ConfTot$MaxX <- max(ConfTot[ConfTot$Alpha.Max ==
# 	                            max(ConfTot$Alpha.Max), "N.Item"])
# 	          ConfTot$MaxY <- max(ConfTot[ConfTot$Alpha.Max ==
# 	                                  max(ConfTot$Alpha.Max), "Alpha.Max"]) + 0.05
# 	          ConfTot$MinY <- min(ConfTot[ConfTot$Alpha.Max ==
# 	                                  min(ConfTot$Alpha.Max), "Alpha.Max"])
# 
# 
# 	          CMCtest <- ggplot(ConfTot, aes(x = N.Item, y = Alpha.Max))
# 	          CMCtest <- CMCtest + geom_point()
# 	          CMCtest <- CMCtest + geom_text(aes(label = Removed.Item, angle
# 	                                             = 45,
# 	                                         hjust = -0.2, vjust = -0.2),
# 	                                     size = 2.5)
# 	          CMCtest <- CMCtest + geom_linerange(aes(x = MaxX, ymin = MinY,
# 	                                              ymax = MaxY) )
# 	          CMCtest <- CMCtest + scale_x_reverse("Número de ítems") + ylab("Curva de Cronbach-Mesbah")
# 	          CMCPlotpng <- file.path(outPath, paste("graficos/CMCGrInc-", indexData, ".png", sep = ""))
# 	          CMCPloteps <- file.path(outPath, paste("graficos/CMCGrInc-", indexData, ".eps", sep = ""))
# 	          # #
# 	          if (!file.exists(CMCPlotpng)) {
# 	            ggsave(CMCPloteps)
# 	            ggsave(CMCPlotpng)
# 	          }
# 
# 	          # # Organizando tabla para exportar ConfTot
# 	          encConfTot     <- c("N.Item", "Alpha.Max", "Removed.Item")
# 	          ConfTot        <- ConfTot[order(ConfTot[, "N.Item"], decreasing =
# 	                                      TRUE),  encConfTot]
# 	          names(ConfTot) <- c("N.Ítem", "Alpha.Máx", "Ítem.Removido")
# 
# 	          # # ICC curve
# 
# 	          resBlock[,"consLect"]  <- personDataBlo[!isMissingHalf]
# 	          resBlock[, "iSubject"] <- 1:nrow(resBlock)
# 
# 	          dirPlotICCeps <- file.path(outPath,
# 	                                   paste("graficos/plotICC-", indexData,
# 	                                         ".eps", sep = ""))
# 
# 	          dirPlotICCpng <- file.path(outPath,
# 	                                   paste("graficos/plotICC-", indexData,
# 	                                         ".png", sep = ""))
# 
# 	          plotICCP(itemParameters, resBlock, personAbilities,
# 	                 scaleD = constDmodel,
# 	                 methodBreaks = "Sturges", nameIndice = ii,
# 	                 dirPlot = c(dirPlotICCeps, dirPlotICCpng), plotName =
# 	                 paste("CCI para el índice", ii))
# 
# 	          # # Person Item - Map
# 	          parItems           <- itemParameters[, c(6, grep("step",
# 	                                                         names(itemParameters)))]
# 	          rownames(parItems) <- itemParameters[, "itemName"]
# 	          model     <- subset(dictionaryList$model,
# 	                            codMod == unique(codModels))[, "model"]
# 	          infoModel <- list('threshtable' = parItems,
# 	                          'X' = resBlock[, indexItems], 'model' = model,
# 	                          'personHabi' = personAbilities[, "ability"])
# 
# 
# 	          dirPerItem <- file.path(outPath, paste("graficos/personItem-",
# 	                                               indexData, ".png", sep = ""))
# 	          png(dirPerItem, res = 72*2, width = 480*2, height = 480*2)
# 	          plotPI(main = paste("Mapa Personas-Items (", ii, ")", sep = ""),
# 	               latdim = "Dimensión latente", object = infoModel, sorted = TRUE,
# 	               scaleD = constDmodel, pplabel = "Distribución\n habilidades",
# 	               discrimination = itemParameters[, "discrimination"],
# 	               thresholdDiff = 0.5, histogram =  TRUE)
# 	          dev.off()
# 
# 	          dirPerItem <- file.path(outPath, paste("graficos/personItem-",
# 	                                               indexData, ".eps", sep = ""))
# 	          postscript(dirPerItem)
# 	          plotPI(main = paste("Mapa Personas-Items (", ii, ")", sep = ""),
# 	               latdim = "Dimensión latente", object = infoModel, sorted = TRUE,
# 	               scaleD = constDmodel, pplabel = "Distribución\n habilidades",
# 	               discrimination = itemParameters[, "discrimination"],
# 	               thresholdDiff = 0.5, , histogram =  TRUE)
# 	          dev.off()
# 
# 	          ################################################################################
# 	          # # Information Plot
# 	          ################################################################################
# 
# 	          # # Tabla de información por ítem (forma vertical)
# 	          itemInfoFile    <- paste("salidas/", indexData, ".PSLII", sep = "")
# 	          itemInformation <- ReadPsInfoFile(fileName = itemInfoFile,
# 	                                          filePath = outPath)
# 
# 	          itemInformation[, "iItem"]         <- factor(itemInformation[, "iItem"])
# 	          levels(itemInformation[, "iItem"]) <- indexItems
# 
# 	          # # Gráfico curva de información por ítem
# 	          infoItemPlot <- ggplot(itemInformation, aes(x = measureQuadPoint,
# 	                                                y = information,
# 	                                                colour = iItem)) +
# 	                    geom_line(aes(colour = iItem), size = 1)  +
# 	                    labs(x = "Dimensión latente", y = "Información",
# 	                         title = paste("Curvas Información Ítems (Índice ", ii, ")", sep =
# 	                                       ""), colour = "Ítem") + theme_bw()
# 
# 	          dirInItemeps <- file.path(outPath, paste("graficos/infoItem-",
# 	                                                   indexData, ".eps", sep = ""))
# 	          dirInItempng <- file.path(outPath, paste("graficos/infoItem-",
# 	                                                   indexData, ".png", sep = ""))
# 
# 	          ggsave(dirInItemeps, width = 10)
# 	          ggsave(dirInItempng, width = 10)
# 
# 	          #####################################################################
# 	          # # Gráfico curva de información para el índice (total) y
# 	          # # curvas sin cada uno de los ítems
# 	          #####################################################################
# 
# 	          # # Tabla de información por ítem (forma horizontal),
# 	          tabInformation <- reshape(itemInformation[,
# 	                                  c("measureQuadPoint", "information",
# 	                                    "iItem")],
# 	                                   idvar =  c("measureQuadPoint"),
# 	                                   timevar = "iItem", direction =
# 	                                   "wide")
# 
# 	          # # Obteniendo la información del índice (suma de la informacion
# 	          # # de los ítems)
# 	          tabInformation[["information"]] <- rowSums(tabInformation[,
# 	                                                     !(names(tabInformation)%in%c("measureQuadPoint"))],
# 	          na.rm = TRUE)
# 
# 	          tabInformation[["item"]] <- "Índice"
# 
# 	          tabGraphInfo <- tabInformation[, c("measureQuadPoint",
# 	                                             "item", "information")]
# 
# 	          tabGraphInfo[["size"]] <- 2
# 
# 	          indexItemsInfo <- c(paste("information.", indexItems, sep = ""))
# 	          # # Obteniendo información del índice sin el ítem
# 	          # # qq = indexItemsInfo[1]
# 
# 	          for(qq in indexItemsInfo){
# 	              tabInformationNo <- tabInformation[, !(names(tabInformation)
# 	                                               %in% "item")]
# 	              tabInformationNo[["item"]] <- paste("Sin", gsub("information.", "\\1",
# 	                                                 qq), sep = " ")
# 	              tabInformationNo[, "informationNo"] <-
# 	                tabInformationNo[["information"]] - tabInformationNo[[qq]]
# 	              tabInformationNo <- tabInformationNo[, !(names(tabInformation)
# 	                                               %in% "information")]
# 	              names(tabInformationNo)[names(tabInformationNo) ==
# 	                                      "informationNo"] <-  "information"
# 	              tabInformationNo <- tabInformationNo[, c("measureQuadPoint",
# 	                                             "item", "information")]
# 	              tabInformationNo [["size"]] <- 1
# 	              tabGraphInfo <- rbind(tabGraphInfo, tabInformationNo)
# 
# 	              rm(tabInformationNo)
# 	          }
# 
# 	          # # Gráfico curva de información para el índice, más
# 	          # # información sin cada ítem
# 	          tabGraphInfo1 <- subset(tabGraphInfo, item != "Índice")
# 	          tabGraphInfo2 <- subset(tabGraphInfo, item == "Índice")
# 
# 	          infoIndicePlot <- ggplot(tabGraphInfo1, aes(x = measureQuadPoint,
# 	                                                y = information,
# 	                                                colour = item)) +
# 	                    geom_line(size = 1) + geom_line(data =
# 	                                                     tabGraphInfo2, size
# 	                                                     = 2) +
# 	                    labs(x = "Dimensión latente", y = "Información",
# 	                         title = paste("Curva Información Índice",
# 	                                           ii, sep = " "),
# 	                         colour = "Ítem") + theme_bw()
# 
# 	          dirInIndiceeps <- file.path(outPath,
# 	                                      paste("graficos/infoIndice-", indexData, ".eps", sep = ""))
# 	          dirInIndicepng <- file.path(outPath,
# 	                                      paste("graficos/infoIndice-", indexData, ".png", sep = ""))
# 
# 	          ggsave(dirInIndiceeps, width = 10)
# 	          ggsave(dirInIndicepng, width = 10)
# 
# 	          rm(tabInformation)
# 
# 	          #####################################################################
# 	          # # Para obtener las tablas con las áreas de las curvas de
# 	          #  # información en el intervalo aproximado [-3, 3]
# 	          #####################################################################
# 
# 	          # # Tabla de información por ítem (forma horizontal),
# 	          tabInformation <- reshape(itemInformation[,
# 	                                  c("measureQuadPoint", "information",
# 	                                    "iItem")],
# 	                                   idvar =  c("measureQuadPoint"),
# 	                                   timevar = "iItem", direction =
# 	                                   "wide")
# 
# 	          # # Obteniendo la información del índice (suma de la informacion
# 	          # # de los ítems)
# 	          tabInformation[["information"]] <- rowSums(tabInformation[,
# 	                                                     !(names(tabInformation)%in%c("measureQuadPoint"))], na.rm = TRUE)
# 
# 	          longInter <- array(0, (nrow(tabInformation) - 1))
# 
# 	          for(ll in c(2:nrow(tabInformation))) {
# 	            longInter[ll-1] <- round(tabInformation[ll, "measureQuadPoint"] -
# 	                               tabInformation[ll-1, "measureQuadPoint"], 2)
# 	          }
# 
# 	          longInter <- unique(longInter)
# 
# 	          isTamUnico <- length(longInter) == 1
# 
# 	          if(isTamUnico) {
# 	            tabPersonAbil  <- cut(personAbilities[,"ability"], c(-Inf,
# 	                                                          tabInformation[,
# 	                                                                        "measureQuadPoint"],
# 	                                                          Inf))
# 
# 	            pTabPersonAbil <-
# 	              as.data.frame(prop.table(table(tabPersonAbil)))
# 
# 	            # # Proporcion de personas por intervalo de habilidad
# 	            tabInformation[["pi"]] <-
# 	              pTabPersonAbil[["Freq"]][-dim(pTabPersonAbil)[1]]
# 
# 	            isPos <-  tabInformation[["measureQuadPoint"]] >= 0
# 
# 	            # # Obteniendo límite superior para el área (mayor aproximación a 3)
# 	            lim    <- tabInformation[isPos, ]
# 	            evalua <- abs(3-lim[["measureQuadPoint"]])
# 	            isMin  <- evalua == min(evalua)
# 	            limSup <- lim[isMin, "measureQuadPoint"]
# 	            rm(lim, evalua, isMin)
# 
# 	            # # Obteniendo límite inferior para el área (mayor
# 	            # aproximación a -3)
# 	            lim    <- tabInformation[!isPos, ]
# 	            evalua <- abs(3+lim[["measureQuadPoint"]])
# 	            isMin  <- evalua == min(evalua)
# 	            limInf <- lim[isMin, "measureQuadPoint"]
# 	            rm(lim, evalua, isMin)
# 
# 
# 	            if(length(limSup) == 1 & length(limInf) == 1) {
# 	              limArea <- paste("(", round(limInf, 2), ",", round(limSup,
# 	                                 2), "]", sep = "")
# 
# 	              isArea <- limInf <= tabInformation[["measureQuadPoint"]] &
# 	                         tabInformation[["measureQuadPoint"]] <= limSup
# 
# 	              tabInformation <- tabInformation[isArea, ]
# 
# 	              indexItemsInfo <- c("information", paste("information.",
# 	                                                       indexItems, sep =
# 	                                                       ""))
# 	              nTabInfo <- nrow(tabInformation)
# 	              # #               qq = indexItemsInfo[1]
# 
# 	              # # Obteniendo información del índice sin el ítem
# 	              for(qq in indexItemsInfo){
# 
# 	                isInformation <- qq != "information"
# 
# 	                if (isInformation) {
# 	                  colInfoSinItem                    <- paste("No", qq, sep = "")
# 	                  tabInformation[[colInfoSinItem]]  <-
# 	                           tabInformation[["information"]] - tabInformation[[qq]]
# 	                }
# 	              }
# 
# 	              indexItemsInfo <- c(indexItemsInfo, paste("No",
# 	                                                        indexItemsInfo[!(indexItemsInfo%in%"information")],
# 	                                                        sep = ""))
# 
# 	              for(qq in indexItemsInfo){
# 	                # # Donde se guarda el dato del área de la CI por punto
# 	                colAreaPoint                    <- paste("Ar", qq, sep = "")
# 	                tabInformation[[colAreaPoint]]  <- 0
# 	                # # Donde se guarda el dato del área*pi de la CI por punto
# 	                colAreaPdPoint                   <- paste("ArP", qq, sep = "")
# 	                tabInformation[[colAreaPdPoint]] <- 0
# 
# 	                for(ll in c(2:(nTabInfo))) {
# 	                  tabInformation[ll , colAreaPoint] <- longInter *
# 	                                            ((tabInformation[ll , qq] +
# 	                                             tabInformation[ll-1 , qq])/2)
# 	                  tabInformation[ll , colAreaPdPoint] <-
# 	                    tabInformation[ll , colAreaPoint] * tabInformation[ll , "pi"]
# 	                }
# 	              }
# 
# 	              areaInfo <- colSums(tabInformation[,
# 	                      !(names(tabInformation)%in%c("measureQuadPoint",
# 	                                                 "pi",indexItemsInfo))])
# 	              areaInfo <- as.data.frame(areaInfo)
# 
# 	              areaInfo[["item"]] <- gsub(".*[.](\\d+).*", "\\1", rownames(areaInfo))
# 	              areaInfo[["item"]] <- gsub(".*(information).*", "--", areaInfo[["item"]])
# 
# 	              # # Identificando si es área total o ponderada de la CI
# 	              # # por ítem
# 	              isPonderada <- grepl("P", rownames(areaInfo), perl = TRUE)
# 	              areaInfo[isPonderada, "area"]  <- "Ponderada"
# 	              areaInfo[!isPonderada, "area"] <- "Total"
# 
# 	              # # Paso para construcción de tabla con área de la CI sin
# 	              # # ítem
# 	              row.names(areaInfo) <-
# 	                ifelse(row.names(areaInfo)%in%c("Arinformation",
# 	                                                "ArPinformation"),
# 	                                            paste(rownames(areaInfo), "No", sep
# 	                                                  = "-"),
# 	                                            row.names(areaInfo))
# 
# 	              isInfoSinItem <- grepl("No", rownames(areaInfo), perl = TRUE)
# 	              isInfoSinItem
# 
# 	              # # Tabla con el área de la CI por ítem aprox intervalo [-3,3]
# 	              areaCI <- areaInfo[!isInfoSinItem, ]
# 	              areaCI <- reshape(areaCI, idvar = "item",
# 	                timevar = "area", direction = "wide")
# 	              names(areaCI) <- c("Ítem", "Total", "Ponderada (*)")
# 
# 	              # # Dividiendo sobre el número de intervalos para poder
# 	              # # comparar con el área ponderada
# 	              areaCI[["Total"]] <- areaCI[["Total"]] / (nTabInfo-1)
# 
# 	              # # Tabla con el área de la CI del índice sin el ítem
# 	              # # aprox intervalo [-3,3]
# 	              areaCIsinItem <- areaInfo[isInfoSinItem, ]
# 	              areaCIsinItem <- reshape( areaCIsinItem, idvar = "item",
# 	                timevar = "area", direction = "wide")
# 	              names(areaCIsinItem) <- c("Ítem.Removido", "Total", "Ponderada (*)")
# 
# 	              # # Dividiendo sobre el número de intervalos para poder
# 	              # # comparar con el área ponderada
# 	              areaCIsinItem[["Total"]] <- areaCIsinItem[["Total"]] / (nTabInfo-1)
# 
# 	            } else {
# 	              stop("OJO ALGÚN LÍMITE NO ES ÚNICO, implementar solución")
# 	            }
# 
# 	          } else {
# 	           cat("Longitud de los intervalos en la función de información
# 	               no es constante, para la prueba ", kk, " y el índice ", ii)
# 	          }
# 
# 
# 
# 	        } else {
# 	        cat ("Para el indice ", ii, " de la prueba ", kk,
# 	             " No se pudo calcular el ajuste de los ítems.")
# 
# 	        isErrorItemFit <- TRUE
# 
# 	        }
# 	      } else {
# 	        stop(">>Error en ", kk)
# 	      }
# 	     } # end if that test whether PH2 file it's right generated
# 	      } # end if ACC model 03
# 
#   	      if (!isReporte & unique(codModels) %in% "03") {
# 	      # # guardar xlsx con las salidas
# 	      outFile <- file.path(outPath,
# 	                        paste('06IRT_', saber, '_pba', kk, indexData, ".xlsx", sep = ''))
# 
# 	      # # require(xlsx)  # # 0.5.5
# 	      wb <- createWorkbook()
# 
# 	      # # estilo de las celdas
# 	      # # estilo del encabezado
# 	      csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) +
# 	              Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
# 	      csEncSB <- CellStyle(wb) + Font(wb, isBold = TRUE) + Alignment(h = "ALIGN_CENTER")
# 	      # # estilo de columnas que reportan porcentajes
# 	      csPor <- CellStyle(wb) + DataFormat("0.0%")
# 	      # # estilo de columnas que reportan la desviación estándar del por
# 	      csDs <- CellStyle(wb) + DataFormat("(0.0%)") +
# 	            Alignment(v = "VERTICAL_CENTER") + Border()
# 	      csDsE <- CellStyle(wb) + Alignment(v = "VERTICAL_CENTER", wrapText = TRUE) +
# 	           Border()
# 	      # # estilo de columnas que reportan n
# 	      csN <- CellStyle(wb) + DataFormat("#,##0.00") + Font(wb, isItalic = TRUE)
# 	      csNE <- CellStyle(wb) + Font(wb, isItalic = TRUE)
# 	      # # estilo de columnas que reportan error estandar de las estimaciones
# 	      csEE <- CellStyle(wb) + DataFormat("(#,##0.000)")
# 	      # # borde
# 	      csPC <- CellStyle(wb) + Border() +
# 	            Alignment(v = "VERTICAL_CENTER", wrapText = TRUE)
# 
# 	      # # fuente en negrilla
# 	      csNeg <- CellStyle(wb) + Font(wb, isBold = TRUE)
# 
# 	      # # paleta de colores para las celdas
# 	      colorChi  <- brewer.pal(3,  "Greens")
# 	      colorCorr <- brewer.pal(3, "Blues")
# 	      colorAlpha <- brewer.pal(3, "Reds")
# 
# 	      # # estilo 1 color revisa chi^2 - Verde Oscuro
# 	      csChi1  <- CellStyle(wb) + DataFormat("#,##0.00") +
# 	               Fill(foregroundColor=colorChi[3]) + Font(wb, isItalic = TRUE)
# 	      # # estilo 2 color revisa chi^2 - Verde Claro
# 	      csChi2  <- CellStyle(wb) + DataFormat("#,##0.00") +
# 	              Fill(foregroundColor=colorChi[2]) + Font(wb, isItalic = TRUE)
# 
# 	      # # estilo color revisa Correlación - Azul
# 	      csCorr1  <- CellStyle(wb) + DataFormat("#,##0.00") +
# 	              Fill(foregroundColor=colorCorr[3]) + Font(wb, isItalic = TRUE)
# 
# 	      # # estilo 1 color revisa Alpha de Cronbach - Rojo Claro
# 	      csAlp1  <- CellStyle(wb) + DataFormat("#,##0.00") +
# 	              Fill(foregroundColor=colorAlpha[2]) + Font(wb, isItalic = TRUE)
# 
# 	      # # estilo 2 color revisa Alpha de Cronbach - Rojo Oscuro
# 	      csAlp2  <- CellStyle(wb) + DataFormat("#,##0.00") +
# 	              Fill(foregroundColor=colorAlpha[3]) + Font(wb, isItalic = TRUE)
# 
# 	      # # estilo auxiliar, escribe en rojo si aparece un error en PH2
# 	      csColRedAuxPH2 <- CellStyle(wb) +
# 	                        Font(wb, isBold = TRUE, heightInPoints = 10,
# 	                             color = "red")
# 
# 	      ################################################
# 	      # # Función con el encabezado común para todas las hojas en el
# 	      # # archivo de salida salida
# 	      ################################################
# 	      encabezado <- function(pruebasDesc, ii, indexItems, nameSheet, csNeg,
# 	                         isCensal, isOmissDel, resBlock, isIndexRasch, codModels,
# 	                         codesRasch, codesNotRasch) {
# 
# 	      namesPrueba <- subset(pruebasDesc, codigo_prueba == kk,
# 	                          select = c(codigo_prueba, prueba))
# 
# 	      namesPrueba[, 'indice'] <- ii
# 	      namesPrueba[, 'nItems'] <- length(indexItems)
# 
# 	      addDataFrame(t(namesPrueba), sheet = get(nameSheet), startRow = 1,
# 	               startColumn = 1, row.names = TRUE,
# 	               col.names = FALSE, rownamesStyle = csNeg)
# 
# 	      criterios <- data.frame(criterio = c('Tipo Análisis', 'Uso criterio omisiones'),
# 	                          valor = c(ifelse(isCensal, 'Censal',
# 	                                           'Muestra Nacional'),
# 	                                    ifelse(isOmissDel, 'Sí', 'No')))
# 
# 	      addDataFrame(criterios, sheet = get(nameSheet), startRow = 1,
# 	               startColumn = 4, row.names = FALSE,
# 	               col.names = FALSE,
# 	               colStyle = list('1' = csNeg))
# 
# 	      # # Identificando el tamaño de la población con la que se realizo
# 	      # # el análisis
# 	      nObsBlock <- nrow(resBlock)
# 	      tamAnal <- data.frame(criterio = 'n Análisis', valor = nObsBlock)
# 	      addDataFrame(tamAnal, sheet = get(nameSheet), startRow = 2,
# 	               startColumn = 6, row.names = FALSE,
# 	               col.names = FALSE,
# 	               colStyle = list('1' = csNeg))
# 
# 	      # # Identificando el modelo en el encabezado
# 	      mod <- unique(codModels)
# 
# 	      if(isIndexRasch) {
# 	        modLabel <- names(codesRasch)[codesRasch == mod]
# 	      } else {
# 	        modLabel <- names(codesNotRasch)[codesNotRasch == mod]
# 	      }
# 
# 	      modelo <- data.frame(criterio = 'modelo', valor =
# 	                            modLabel)
# 	      addDataFrame(modelo, sheet = get(nameSheet), startRow = 3,
# 	               startColumn = 4, row.names = FALSE,
# 	               col.names = FALSE,
# 	               colStyle = list('1' = csNeg))
# 
# 	      setColumnWidth(get(nameSheet), 1, 15)
# 	      }
# 
# 	    if(isErrorItemFit) {
# 
# 	    # # Hoja 1:
# 	    nameSheet <- "Resultados"
# 
# 	    assign(nameSheet, createSheet(wb, sheetName = nameSheet))
# 
# 	    encabezado(pruebasDesc, ii, indexItems, nameSheet, csNeg,
# 	                       isCensal, isOmissDel, resBlock, isIndexRasch, codModels,
# 	                       codesRasch, codesNotRasch)
# 
# 	    if(testPH2OK$error) {
# 	    info <-  paste("Para el indice", ii, "de la prueba", kk, "no se pudo
# 	                   calcular el ajuste de los ítems. Revise la siguiente
# 	                   salida y determine que ítem esta ocasionando problemas de estimación")
# 	        } else {
# 	      info <-  paste("Para el indice", ii, "de la prueba", kk, "no se pudo calcular el ajuste de los ítem")
# 	      }
# 
# 	    # # poner el data.frame en la hoja
# 	    addDataFrame(info, sheet = get(nameSheet), startRow = 6,
# 	               startColumn = 1, row.names = FALSE,
# 	               col.names = FALSE)
# 
# 	    if(testPH2OK$error) {
# 	    addDataFrame(testPH2OK$fragment, sheet = get(nameSheet), startRow = 10,
# 	               startColumn = 1, row.names = FALSE,
# 	               col.names = FALSE)
# 	                }
# 
# 	    saveWorkbook(wb, file = outFile)
# 
# 	    outPath <- auxOutPath
# 
# 	    } else {
# 
# 	     # # Hoja 1: Items
# 	    nameSheet <- "Items"
# 
# 	    tableItems <- subset(selectedModel, indice == ii)
# 
# 	    isVacio <- tableItems[, 'etiqu'] == '' | is.na(tableItems[, 'etiqu'])
# 
# 	    if (any(isVacio)) {
# 	      tableItems[isVacio, 'etiqu'] <- tableItems[isVacio, 'instr']
# 	    }
# 
# 	    tableItems <- tableItems[ , c("id", "etiqu")]
# 	    names(tableItems) <- c("Ítems", "Etiqueta")
# 
# 	    assign(nameSheet, createSheet(wb, sheetName = nameSheet))
# 
# 	    encabezado(pruebasDesc, ii, indexItems, nameSheet, csNeg,
# 	                       isCensal, isOmissDel, resBlock, isIndexRasch, codModels,
# 	                       codesRasch, codesNotRasch)
# 
# 	    # # poner el data.frame en la hoja
# 	    addDataFrame(tableItems, sheet = get(nameSheet), startRow = 6,
# 	               startColumn = 1, row.names = FALSE,
# 	               col.names = TRUE, colnamesStyle = csEnc)
# 
# 	    # #     anchoEtiq <- max(nchar(tableItems[ , "Etiqueta"]))
# 
# 	    setColumnWidth(get(nameSheet), 2, 30)
# 
# 	    # # Hoja 2: Confiabilidad
# 	    nameSheet <- "Confiabilidad"
# 
# 	    assign(nameSheet, createSheet(wb, sheetName = nameSheet))
# 
# 	    encabezado(pruebasDesc, ii, indexItems, nameSheet, csNeg,
# 	                       isCensal, isOmissDel, resBlock, isIndexRasch, codModels,
# 	                       codesRasch, codesNotRasch)
# 
# 	    # # poner el data.frame en la hoja
# 	    addDataFrame(ConfTot, sheet = get(nameSheet), startRow = 6,
# 	               startColumn = 1, row.names = FALSE,
# 	               col.names = TRUE, colnamesStyle = csEnc,
# 	               colStyle = list('2' = csN, '5' = csN, '6' = csN))
# 
# 	    setColumnWidth(get(nameSheet), 2, 14)
# 	    setColumnWidth(get(nameSheet), 3, 14)
# 	    setColumnWidth(get(nameSheet), 4, 14)
# 
# 	    addPicture(file = paste(outPath,"/graficos/CMCGrInc-",
# 	                                               indexData, ".png", sep = ""),
# 	             sheet = get(nameSheet), scale = 0.8,
# 	              startRow = 6, startColumn = 5)
# 
# 	    # # Hoja 3: Curva característica de los ítems
# 
# 	    nameSheet <- "CCI"
# 
# 	    assign(nameSheet, createSheet(wb, sheetName = nameSheet))
# 
# 	    encabezado(pruebasDesc, ii, indexItems, nameSheet, csNeg,
# 	                       isCensal, isOmissDel, resBlock, isIndexRasch, codModels,
# 	                       codesRasch, codesNotRasch)
# 
# 	    addPicture(file = paste(outPath,"/graficos/plotICC-",
# 	                                               indexData, ".png", sep = ""),
# 	             sheet = get(nameSheet), scale = 0.8,
# 	              startRow = 6, startColumn = 2)
# 
# 	    setColumnWidth(get(nameSheet), 4, 14)
# 
# 	    # # Hoja 4: Curvas de la función de información de los ítems
# 
# 	    nameSheet <- "FI"
# 
# 	    assign(nameSheet, createSheet(wb, sheetName = nameSheet))
# 
# 	    encabezado(pruebasDesc, ii, indexItems, nameSheet, csNeg,
# 	                       isCensal, isOmissDel, resBlock, isIndexRasch, codModels,
# 	                       codesRasch, codesNotRasch)
# 
# 	    # # Sumando tabla con las áreas curvas de información
# 	    titleArea <- data.frame(tArea = 'Área aproximada - Curvas Información Ítems')
# 	    titleLim  <- data.frame(tLim = paste("Intervalo evaluación:", limArea, sep = " "))
# 
# 	    addDataFrame(titleArea, sheet = get(nameSheet),
# 			     startRow = 6, startColumn = 2,
# 	                 row.names = FALSE, col.names = FALSE, colStyle =
# 	                 list('1' = csEncSB))
# 
# 	    addMergedRegion(sheet = get(nameSheet),
# 	                    startRow = 6, endRow = 6,
# 	                    startColumn = 2, endColumn = 4)
# 
# 	    addDataFrame(titleLim, sheet = get(nameSheet),
# 			     startRow = 7, startColumn = 2,
# 	                 row.names = FALSE, col.names = FALSE, colStyle =
# 	                 list('1' = csEncSB))
# 
# 	    addMergedRegion(sheet = get(nameSheet),
# 	                    startRow = 7, endRow = 7,
# 	                    startColumn = 2, endColumn = 4)
# 
# 	    addDataFrame(areaCI, sheet = get(nameSheet), startRow = 8,
# 	               startColumn = 2, row.names = FALSE,
# 	               col.names = TRUE, colnamesStyle = csEnc,
# 	               colStyle = list('2' = csN, '3' = csN))
# 
# 	    # # Sumando tabla con las áreas curvas de información
# 	    titleArea <- data.frame(tArea = "Área aproximada - Curva Información Índice sin el Ítem")
# 	    titleLim  <- data.frame(tLim = paste("Intervalo evaluación:", limArea, sep = " "))
# 	    pond      <- data.frame(pond = "(*) Ponderada por la proporción de personas según nivel de habilidad")
# 
# 	    addDataFrame(titleArea, sheet = get(nameSheet),
# 			     startRow = (10 + length(indexItems)), startColumn = 2,
# 	                 row.names = FALSE, col.names = FALSE, colStyle =
# 	                 list('1' = csEncSB))
# 
# 	    addMergedRegion(sheet = get(nameSheet),
# 	                    startRow = (10 + length(indexItems)), endRow = (10 + length(indexItems)),
# 	                    startColumn = 2, endColumn = 4)
# 
# 	    addDataFrame(titleLim, sheet = get(nameSheet),
# 			     startRow = (11 + length(indexItems)), startColumn = 2,
# 	                 row.names = FALSE, col.names = FALSE, colStyle =
# 	                 list('1' = csEncSB))
# 
# 	    addMergedRegion(sheet = get(nameSheet),
# 	                    startRow = (11 + length(indexItems)), endRow = (11 + length(indexItems)),
# 	                    startColumn = 2, endColumn = 4)
# 
# 	    addDataFrame(areaCIsinItem, sheet = get(nameSheet), startRow = (12 + length(indexItems)),
# 	               startColumn = 2, row.names = FALSE,
# 	               col.names = TRUE, colnamesStyle = csEnc,
# 	               colStyle = list('2' = csN, '3' = csN))
# 
# 	    addDataFrame(pond, sheet = get(nameSheet),
# 			     startRow = (15 + (2*length(indexItems))), startColumn = 2,
# 	                 row.names = FALSE, col.names = FALSE)
# 
# 	    addMergedRegion(sheet = get(nameSheet),
# 	                    startRow = (15 + (2*length(indexItems))), endRow =
# 	                    (15 + (2*length(indexItems))),
# 	                    startColumn = 2, endColumn = 4)
# 
# 	    # # Sumando grafico con las curvas de información de los ítems
# 	    addPicture(file = paste(outPath,"/graficos/infoItem-",
# 	                                               indexData, ".png", sep = ""),
# 	             sheet = get(nameSheet), scale = 0.8,
# 	              startRow = 6, startColumn = 6)
# 
# 	    # # Sumando gráfico con la curva de información del índice
# 	    addPicture(file = paste(outPath,"/graficos/infoIndice-",
# 	                                               indexData, ".png", sep = ""),
# 	             sheet = get(nameSheet), scale = 0.8,
# 	              startRow = 32, startColumn = 6)
# 
# 	    setColumnWidth(get(nameSheet), 2, 15)
# 	    setColumnWidth(get(nameSheet), 3, 17)
# 	    setColumnWidth(get(nameSheet), 4, 17)
# 
# 
# 	    # # Hoja 5: Mapa personas ítems
# 
# 	    nameSheet <- "Mapa_P-I"
# 
# 	    assign(nameSheet, createSheet(wb, sheetName = nameSheet))
# 
# 	    encabezado(pruebasDesc, ii, indexItems, nameSheet, csNeg,
# 	                       isCensal, isOmissDel, resBlock, isIndexRasch, codModels,
# 	                       codesRasch, codesNotRasch)
# 
# 	    # # sumando tabla con la proporción de personas por habilidad
# 	    titleProp <- data.frame(tArea = "Porcentaje de personas según nivel de habilidad")
# 
# 	    addDataFrame(titleProp, sheet = get(nameSheet),
# 			     startRow = 6, startColumn = 2,
# 	                 row.names = FALSE, col.names = FALSE, colStyle =
# 	                 list('1' = csEncSB))
# 
# 	    addMergedRegion(sheet = get(nameSheet),
# 	                    startRow = 6, endRow = 6,
# 	                    startColumn = 2, endColumn = 6)
# 
# 	    # # sumando tabla con intervalos generales a partir de puntos de la FI
# 	    titleProp1 <- data.frame(tArea = "Intervalos FI")
# 
# 	    addDataFrame(titleProp1, sheet = get(nameSheet),
# 			     startRow = 7, startColumn = 2,
# 	                 row.names = FALSE, col.names = FALSE, colStyle =
# 	                 list('1' = csEncSB))
# 
# 	    addMergedRegion(sheet = get(nameSheet),
# 	                    startRow = 7, endRow = 7,
# 	                    startColumn = 2, endColumn = 3)
# 
# 	    names(pTabPersonAbil) <- c("Habilidad", "%")
# 
# 	    addDataFrame(pTabPersonAbil, sheet = get(nameSheet), startRow = 8,
# 	               startColumn = 2, row.names = FALSE,
# 	               col.names = TRUE, colnamesStyle = csEnc,
# 	               colStyle = list('2' = csPor))
# 
# 	    # # creando tabla de porcentaje de personas por umbral promedio
# 	     if(exists("stepThreshold")){
# 	       # # si los ítems son politómicos (GRM)
# 	       outItemsThr <- apply(outItems[, stepThreshold], 2, mean)
# 	     } else{
# 	       # # si los ítems son dicotómicos (2PL)
# 	       outItemsThr <- mean(outItems[, "Threshold"])
# 	     }
# 
# 	    tabPersonAbilThr  <- cut(personAbilities[,"ability"], c(-Inf,
# 	                                                          outItemsThr,
# 	                                                          Inf))
# 
# 	    ptabPersonAbilThr <- as.data.frame(prop.table(table(tabPersonAbilThr)))
# 
# 	    names(ptabPersonAbilThr) <- c("Habilidad", "%")
# 
# 	    # # sumando tabla con intervalos a partir del promedio de los umbrales
# 	    titleProp2 <- data.frame(tArea = "Intervalos Promedio Umbrales")
# 
# 	    addDataFrame(titleProp2, sheet = get(nameSheet),
# 			     startRow = 7, startColumn = 5,
# 	                 row.names = FALSE, col.names = FALSE, colStyle =
# 	                 list('1' = csEncSB))
# 
# 	    addMergedRegion(sheet = get(nameSheet),
# 	                    startRow = 7, endRow = 7,
# 	                    startColumn = 5, endColumn = 6)
# 
# 	    addDataFrame(ptabPersonAbilThr, sheet = get(nameSheet), startRow = 8,
# 	               startColumn = 5, row.names = FALSE,
# 	               col.names = TRUE, colnamesStyle = csEnc,
# 	               colStyle = list('2' = csPor))
# 
# 
# 	    # # sumando gráfico de mapa personas - ítems
# 	    addPicture(file = paste(outPath,"/graficos/personItem-",
# 	                                               indexData, ".png", sep = ""),
# 	             sheet = get(nameSheet), scale = 0.8,
# 	              startRow = 6, startColumn = 8)
# 
# 	    setColumnWidth(get(nameSheet), 2, 14)
# 	    setColumnWidth(get(nameSheet), 4, 14)
# 	    setColumnWidth(get(nameSheet), 5, 14)
# 
# 	    # # Hoja 6: Parametros
# 
# 	    nameSheet <- "Parametros"
# 
# 	    assign(nameSheet, createSheet(wb, sheetName = nameSheet))
# 
# 	    encabezado(pruebasDesc, ii, indexItems, nameSheet, csNeg,
# 	                       isCensal, isOmissDel, resBlock, isIndexRasch, codModels,
# 	                       codesRasch, codesNotRasch)
# 
# 	    criteriosColor <- data.frame(Col1 = c("", "", "Correlación < 0.2",
# 	                                          "",  "alpha < alpha_{-i} <= (alpha +0.05)",
# 	                                          "(alpha + 0.05) < alpha_{-i}"),
# 	                                 Col2 = c("Si  n Análisis <= 3000",
# 	                                        "1 + (2/(3000/n Análisis)) <= chi^2/gl < 3",
# 	                                        "3 <= chi^2/gl" ,
# 	                                        "Si  n Análisis > 3000",
# 	                                        "3 <= chi^2/gl <  1 + (2/(3000/n Análisis))",
# 	                                        "1 + (2/(3000/n Análisis)) <= chi^2/gl"))
# 
# 	    addDataFrame(criteriosColor, sheet = get(nameSheet), startRow = 1,
# 	               startColumn = 8, row.names = FALSE,
# 	               col.names = FALSE)
# 
# 	    # # Agregar color para criteriosColor
# 	    colRetain <- 8:9
# 	    rowRetain <- 1:6
# 	    rows <- getRows(sheet = get(nameSheet),
# 	                      rowIndex = rowRetain )
# 	    cells <- getCells(rows, colIndex = colRetain)
# 
# 	    posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
# 
# 	    # # Agregar color para criterio correlacion
# 	    lapply(posiciones[3,1], function(x) setCellStyle(cells[[x]], csCorr1))
# 
# 	    # # Agregar color para criterio alpha cronbach
# 	    # # color claro
# 	    lapply(posiciones[5,1], function(x) setCellStyle(cells[[x]], csAlp1))
# 	    # # color oscuro
# 	    lapply(posiciones[6,1], function(x) setCellStyle(cells[[x]], csAlp2))
# 
# 	    # # Agregar color para criterio chi^2
# 	    # # color claro
# 	    lapply(posiciones[c(2,5),2], function(x) setCellStyle(cells[[x]], csChi2))
# 	    # # color oscuro
# 	    lapply(posiciones[c(3,6),2], function(x) setCellStyle(cells[[x]], csChi1))
# 
# 	    # # Definiendo estilo de las columnas
# 
# 	    colEstilo = c("list(", paste("'" , rep(2:(nSteps+6)), "'", " = csN,", sep =
# 	             ""), paste("'" , nSteps+8, "'", " = csN,", sep = ""),
# 	                  paste("'" , rep((nSteps+9):((2*nSteps)+10)), "'", " = csEE,", sep = ""),
# 	                  paste("'" , rep(((2*nSteps)+11):((2*nSteps)+12)), "'",
# 	                        " = csN,", sep = ""),
# 	                   paste("'" , (2*nSteps)+13, "'", " = csN", sep = ""), ")")
# 
# 	    colEstilo1 <- colEstilo[1]
# 	    for(pp in 2:length(colEstilo)) {
# 	      colEstiloDef <- paste(colEstilo1, colEstilo[pp], sep=" ")
# 	      colEstilo1   <- colEstiloDef
# 	    }
# 
# 	    addDataFrame(outItems, sheet = get(nameSheet), startRow = 8,
# 	               startColumn = 1, row.names = FALSE,
# 	               col.names = TRUE, colnamesStyle = csEnc,
# 	               colStyle = eval(parse(text = colEstiloDef)))
# 
# 	    # # Configurando ancho de las columnas en el archivo xlsx
# 	    for(cc in c(2:(nSteps + 4), 8:9, rep((nSteps+9):((2*nSteps)+13)))) {
# 	      setColumnWidth(get(nameSheet), cc, 15)
# 	    }
# 
# 	    # # Agregar color para resultados chi^2
# 	    colRetain <- nSteps + 8
# 	    rowRetain <- c(9:(nrow(outItems)+8))
# 	    rows <- getRows(sheet = get(nameSheet),
# 	                      rowIndex = rowRetain )
# 	    cells <- getCells(rows, colIndex = colRetain)
# 
# 	    posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
# 
# 
# 	    nObsBlock <- nrow(resBlock)
# 
# 	    if ( nObsBlock <= 3000 ) {
# 	      chi1 <- outItems[, "chi^2/gl"] >= 3
# 	      chi2 <- kThresItemChiHigh <=  outItems[, "chi^2/gl"] & outItems[, "chi^2/gl"] < 3
# 
# 	      pcChi1 <- posiciones[chi1]
# 	      pcChi2 <- posiciones[chi2]
# 
# 	      # # Marcando color claro
# 	      lapply(pcChi2, function(x) setCellStyle(cells[[x]], csChi2))
# 	      # # Marcando color oscuro
# 	      lapply(pcChi1, function(x) setCellStyle(cells[[x]], csChi1))
# 	    } else {
# 	      chi1 <- outItems[, "chi^2/gl"] >= kThresItemChiHigh
# 	      chi2 <- 3 <=  outItems[, "chi^2/gl"] & outItems[, "chi^2/gl"] < kThresItemChiHigh
# 
# 	      pcChi1 <- posiciones[chi1]
# 	      pcChi2 <- posiciones[chi2]
# 
# 	      # # Marcando color claro
# 	      lapply(pcChi2, function(x) setCellStyle(cells[[x]], csChi2))
# 	      # # Marcando color oscuro
# 	      lapply(pcChi1, function(x) setCellStyle(cells[[x]], csChi1))
# 	    }
# 
# 	    # # Agregar color para resultados Correlación
# 	    colRetain <- nSteps + 4
# 	    rowRetain <- c(9:(nrow(outItems)+8))
# 	    rows <- getRows(sheet = get(nameSheet),
# 	                      rowIndex = rowRetain )
# 	    cells <- getCells(rows, colIndex = colRetain)
# 
# 	    posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
# 
# 	    corr1 <- outItems[, "Correlación"] < kThresItemCorr
# 
# 	    pcCorr1 <- posiciones[corr1]
# 
# 	    lapply(pcCorr1, function(x) setCellStyle(cells[[x]], csCorr1))
# 
# 	     # # Agregar color para resultados Alpha de Cronbach
# 	    colRetain <- nSteps + 5
# 	    rowRetain <- c(9:(nrow(outItems)+8))
# 	    rows <- getRows(sheet = get(nameSheet),
# 	                      rowIndex = rowRetain )
# 	    cells <- getCells(rows, colIndex = colRetain)
# 
# 	    posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
# 
# 	    alp1 <- alphaCoef < outItems[, "alpha_{-i}"] &
# 	                              outItems[, "alpha_{-i}"] <= (alphaCoef +
# 	                                                           0.05)
# 	    alp2 <- (alphaCoef + 0.05) < outItems[, "alpha_{-i}"]
# 
# 	    pcAlp1 <- posiciones[alp1]
# 	    pcAlp2 <- posiciones[alp2]
# 
# 	    lapply(pcAlp1, function(x) setCellStyle(cells[[x]], csAlp1))
# 	    lapply(pcAlp2, function(x) setCellStyle(cells[[x]], csAlp2))
# 
# 	    saveWorkbook(wb, file = outFile)
# 
# 	    outPath <- auxOutPath
# 
# 	    }
# 	    }
# 	    outPath <- auxOutPath
	    }
	  }
   }
  }
)

resulIRT <- list()
for(qq in names(controlData)){
	resulIRT[[qq]] <- analIRT(controlData[[qq]])
}

resulModel <- lapply(resulIRT, function(x) x[["datosReport"]])
save(resulModel, file = "Output\\06IRT\\granDatosReporte.Rdata")	

resulMapa <- lapply(resulIRT, function(x) x[["mapa"]])
save(resulMapa, file = "Output\\06IRT\\mpITPERS.Rdata")	
