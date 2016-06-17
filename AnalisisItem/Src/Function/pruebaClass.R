################################################################################
# # pruebaClass.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Jorge Mario Carrasco, Nelson Rodriguez, William Acero, 
# #            Robert Romero
# #
# # SABER 359, SABER PRO, SABER 11 y ACC
# # Description: Función creada para definar la clase test, la clase análisis y
# #              reporte test para las definir las funciones de análisis de item.
# #
# # Outputs: Funciones para crear objetos y metodos
# #
# # File history:
# #   20150914: Creation
# #   20160516: Modificación de las clases test y Análisis
# # ToDo:
# #       Inclusión de la definición de las clases y los metodos
################################################################################

################################################################################
# # Definición clase Test
################################################################################

Test <- setClass("Test", 
 		# # Definir la estructura
 		slots = c(path           = 'character',  # Ruta de la carpeta 
 			        dictionaryList = 'list',		   # Diccionario de variables
 			        datBlock       = 'list',  	   # Datos leidos
 			        exam           = 'character',  # (SABER359, SABER11, SABERPRO)
	            verInput       = 'numeric',    # Version con el que se genera diccionarios
	            nomTest        = 'character',  # Nombre para impresion en los reportes
	            paramLect      = 'list',       # Parametros de lectura de la prueba
	            codMod         = 'character',
              listAnal       = 'list'), # Codigos del modelo de la prueba

 		# # Definir los valores por defecto
 		prototype = list(verInput = 1, path = "", exam = "", codMod = "02",
					  	       nomPrueba = "", paramLect = list(conDirs = "")), 
 		validity  = function(object){ 			 			
      auxModelos <- data.frame('codMod' = c("00", "01", "02", "03", "04", "05", "06", "07"),
                               'model'  = c("noModel", "PCM", "RSM", "GRM", "MRD", "2PL", "1PL", "3PL"))		
 			if(!object@codMod %in% auxModelos[, "codMod"]){
 				cat("No existe el codigo del modelo: ", object@codMod)
 				print(auxModelos)
				stop("\nSeleccionar entre los modelos anteriores")
			}
 			if(object@path == "")
 				stop("Se debe definir el directorio del examen")
 			if(object@paramLect$conDirs == "")
 				stop("El parametro 'conDirs' debe contener almenos un .con")
 			return(TRUE)
 		})

# # Methods's Test Class
setGeneric(name = "readSupplies", def = function(object){standardGeneric("readSupplies")})

setMethod("readSupplies", "Test",
function(object){
 
  ################################################################################
  # # Definition of input and output paths
  ################################################################################
  outPath <- file.path("..", "Output", "00Crear")
  dir.create(outPath, showWarnings = FALSE, recursive = TRUE)

  ################################################################################
  # # Source of scripts with functions
  ################################################################################
  source(file.path(funPath, "partirComas.R"), local = TRUE)  
  source(file.path(funPath, "readFilesAI02.R"), local = TRUE)   
  
  ################################################################################
  # # Validation for reading a Test
  ################################################################################
	controlPrueba <- object
	controlAnal   <- object@paramLect

    # # Parameters validation
    if (controlPrueba@exam == "" | is.na(controlPrueba@exam))
	    stop("**ERROR** No se espesifico el nombre de la prueba que se quiere procesar")
    if (controlPrueba@path == "" | is.na(controlPrueba@path))
	    stop("**ERROR** Se debe especificar una ruta para cada prueba")
	  if (!file.exists(file.path(inPath, controlPrueba@path)))
		  stop("**ERROR** El directorio de la prueba no existe")
    if (is.na(controlPrueba@verInput))
	    stop("**ERROR** Se debe especificar la versión de entrada")
    if (!"conDirs" %in% names(controlAnal))
	    stop("**ERROR** Se debe especificar el vector de .con que se quieren Leer")
    if (length(controlAnal$conDirs) == 0)
	    stop("**ERROR** Se debe especificar almenos un .con en los parametros de lectura")
    if (controlPrueba@exam == "ACC"){
	    if (!"Estructura" %in% names(controlAnal))
		    stop("**ERROR** Para procesar una prueba NO cognitiva se debe asignar un diccionario")
	    if (length(controlAnal$Estructura) == 0)
		    stop("**ERROR** Se debe especificar almenos un .con en los parametros de lectura")
    } else {
	    if (!"Estructura" %in% names(controlAnal))
		    warning("\n>>>O_O>>> No se especifico el archivo de Estructura\n",
			        "          el diccionario se creara segun la descarga.\n")
    }

    ################################################################################
    # # global definitions
    ################################################################################

    # # name of Rdata files

    if (controlPrueba@exam == "ACC"){
      datDictionary <- file.path(outPath, 
                           paste("dictionaryList_V",
                                 controlPrueba@verInput, ".RData", sep = ""))
      datReadBlock  <- file.path(outPath, 
                           paste("datBlock_V",
                                 controlPrueba@verInput, ".RData", sep = ""))
    } else {
      datDictionary <- file.path(outPath,
                               paste("dictionaryList_",
                                     gsub(".+(\\\\|\\/)(.+)$", "\\2", controlPrueba@path), "_V",
                                     controlPrueba@verInput, ".RData", sep = ""))
      datReadBlock  <- file.path(outPath,
                               paste("datBlock_",
                                     gsub(".+(\\\\|\\/)(.+)$", "\\2", controlPrueba@path),"_V",
                                     controlPrueba@verInput, ".RData", sep = ""))
    }

    ################################################################################
    # #  reading  dictionary and DB
    ################################################################################
    inFolder <- controlPrueba@path

    if (controlPrueba@exam == "ACC") {
    	if (!file.exists(datDictionary)){
		    cat("Nombre del diccionario - ->", controlAnal$Estructura, "\n")
		    # # Reading the project dictionary
		    dictionaryList <- ReadDict(fileName = controlAnal$Estructura,
                                   variables = controlAnal$nameSheet, 
                                   categories = "OpcResp", model = "model",
	            	                   index = "escalas", collapse = "colapsa",
	                	               desElim = "elimina")
        attr(dictionaryList, "fileRdata") <- datDictionary
		    save(dictionaryList, file = datDictionary)
		  } else {
			  cat("Cargando el diccionario - ->", controlAnal$Estructura, "\n")
		   	load(datDictionary)
		  } 
		  datBlock <- ReadDataAI(folderName = inFolder, dict = dictionaryList,
	                           multiMarkOmiss = TRUE, verbose = TRUE,
	                           eliminatedVars = FALSE)
		  save(datBlock, file = datReadBlock)
    } else {
      # # Reading the project dictionary
      if (!file.exists(datDictionary)){
      	dictionaryList <- con2Dict(object)
        attr(dictionaryList, "fileRdata") <- datDictionary
        save(dictionaryList, file = datDictionary)

      } else {
      	load(datDictionary)
      }
      # # Reading the DB using generic dictionary (only for dichotomous items)
      if (is.null(object@paramLect$valMUO))
        object@paramLect$valMUO <- 9
	    datBlock <- ReadGeneric(object, dict = dictionaryList, verbose =  FALSE, 
                              valMUO = object@paramLect$valMUO, eliminatedVars = FALSE)
	    save(datBlock, file = datReadBlock)
    }    
    # # Save directions of Rdata and datDictionary    
    object@dictionaryList <- dictionaryList
    object@datBlock       <- datBlock

    return(object)
  })


setMethod("initialize", "Test", function(.Object, ..., test) {    
    .Object <- callNextMethod()
    if(missing(test)){
    	if (length(.Object@path) != 0){
    		if (length(.Object@dictionaryList) == 0 & length(.Object@datBlock) == 0){
          		.Object <- readSupplies(.Object)
    		}    	
    	}
    }
    .Object
  })

################################################################################
# # Definición Clase Analysis
################################################################################

Analysis <- setClass("Analysis",  					  
 					    # # Definir la estructura                
 					    slots = c(
 					  	   test        = 'Test',
 					  	   datAnalysis = 'list',
 					  	   param       = 'list', 
 					  	   inputFile   = 'list',
 					  	   outFile     = 'list', 
 					  	   verSalida   = 'numeric', 
                 pathCor     = 'character', 
                 funVirtual  = 'character'),  					  
 					    # # Definir los valores por defecto
 					    prototype = list(test = NULL, scripName = "", param = list(), 
 					                    inputFile = list(), outFile = list(pathRdata = "")), 
 					    validity  = function(object){ 					  	 					  	
 					  	   if (object@outFile$pathRdata == "")
 					  		  return("Se debe definir un directorio de salida de resultados (Rdata)")
 					      return(TRUE)})

setMethod("initialize", "Analysis", function(.Object, ..., test, param, verSalida) {
	.Object <- callNextMethod()
    if(missing(test)){
      stop("Se debe especificar un objeto 'test = '??? ")
    }
    if(missing(param)){
      stop("Se debe definir los parametros de la clase --> ", class(.Object))
    } else {
      .Object@param <- param    
    }
    if(missing(verSalida)){
      .Object@verSalida <- 1
    }
    .Object@test <- test    
    auxClase        <- class(.Object)
    methodAnalysis  <- attr(methods(class = auxClase), "info")[, "generic"]
    if (!"codeAnalysis" %in% methodAnalysis)
      stop("-- Se debe definir el metodo 'codeAnalysis' para la clase --> ", auxClase)
    if (!"outXLSX" %in% methodAnalysis)
      stop("-- Se debe definir el metodo 'outXLSX' para la clase --> ", auxClase)
    if (!"outHTML" %in% methodAnalysis)
      stop("-- Se debe definir el metodo 'outHTML' para la clase --> ", auxClase)
    .Object
  })

# # Metodos de la clase Análisis
setGeneric(name = "getParams", def = function(object){standardGeneric("getParams")})
setMethod("getParams", "Analysis", function(object){return(object@param)})

setGeneric(name = "getInput", def = function(object){standardGeneric("getInput")})
setMethod("getInput", "Analysis", function(object){return(object@inputFile)})

setGeneric(name = "getOutput", def = function(object){standardGeneric("getOutput")})
setMethod("getOutput", "Analysis", function(object){return(object@outFile)})

setGeneric(name = "filterSubsets", 
           def = function(object, recodeNA = FALSE, catToNA = NULL, kOmissionThreshold = 1, 
                          orderedDat = FALSE, kCodNElim = "06", kCodPar = NULL, kCodMod = NULL, 
                          columnId = "SNP", flagNI = FALSE, filPrueba = NULL , 
                          filIndex = NULL, ...){standardGeneric("filterSubsets")})
setMethod("filterSubsets", "Analysis", 
  function(object, ...){
    # # Package in R
    require(plyr)
    require(data.table)

    # # filtering dictionary
    for(blockDat in names(object@datAnalysis)){
      dictionaryList <- object@datAnalysis[[blockDat]]$dictionary
      # # Default values of some fields
      if(!"paralelo" %in% names(dictionaryList)){
        dictionaryList$paralelo <- "01"        
      }

      if(!"subCon" %in% names(dictionaryList)){
        dictionaryList$subCon <- dictionaryList$codigo_prueba        
      } 

      # # Filtering items
      isNoElim   <- dictionaryList[, 'elimina' ] == kCodNElim  

      if (!is.null(kCodMod)){
        isCodMod <- dictionaryList[, 'codMod'] == kCodMod     
      } else {
        isCodMod <- rep(TRUE, nrow(dictionaryList))
      }      
      if (!is.null(kCodPar)){
        isCodPar <- dictionaryList[, 'paralelo'] == kCodPar     
      } else {
        isCodPar <- rep(TRUE, nrow(dictionaryList))
      }   

      if (object@test@exam == "ACC") {
        if (flagNI){
          isNI     <- dictionaryList[, 'subCon'] != 'NI' 
          isPrueba <- isCodMod & isNoElim & isCodPar & isNI
        }
      } else {
        isPrueba   <- isCodMod & isNoElim & isCodPar
      }     
  
      # # Filtering items by 'codigo_prueba' and 'subCon'
      if (!is.null(filPrueba)) {
        flagPru  <- dictionaryList[, 'codigo_prueba'] %in% filPrueba        
        isPrueba <- isPrueba & flagPru
      }
      if (!is.null(filIndex)) {
        flagIndex <- dictionaryList[, 'subCon'] %in% filIndex
        isPrueba  <- isPrueba & flagIndex
      }
      dictionaryList <- dictionaryList[isPrueba, ]
      object@datAnalysis[[blockDat]]$dictionary <- dictionaryList
    }

    # # Conserved data from sample
    if(object@test@exam %in% c("ACC", "SABER359")){
      object@datAnalysis <- lapply(object@datAnalysis, function(x){
                                   x$datos <- subset(x$datos, x$datos$tipoApli %in% object@param$kApli)
                                   x$datos <- data.frame(x$datos)
                                   names(x$datos) <- gsub("X(\\d+)", "\\1", names(x$datos))
                                   return(x)})
    } else {
      object@datAnalysis <- lapply(object@datAnalysis, function(x) {
                                   x$datos <- data.frame(x$datos)
                                   names(x$datos) <- gsub("X(\\d+)", "\\1", names(x$datos))
                                   return(x)})
    }

    RecodeToNA <- function (variable, categories){
     # # Recode the categories of variable to NA
     # #
     # # Arg:
     # #  variable[character|factor]: variable to recode
     # #  categories[character]: levels of variable recoding to NA
     # #
     # # Ret:
     # #  the variable after recode
     # # levels of variable
         if (is.null(categories)) 
           stop("... Se debe definir el parametro 'catToNA'")
         levelsVar <- levels(variable)
         levelsVar <- levelsVar[!(levelsVar %in% categories)]
         isCat     <- variable %in% categories
         if (any(isCat)) {
           variable[isCat] <- NA
         }
         variable <- ordered(variable, levels = levelsVar)
         return(variable)
    } 

    # # Recode 'Multimarca' 'No aplica' 'NR' for NA
    recodeLA <- function(subDat){
        varId  <- intersect(subDat$dict$id, names(subDat$datos))
        if (length(varId) == 0){
          return(NULL)
        } 
        dataBlo <- data.table(subDat$datos[, c(columnId, varId)])       
        if (recodeNA) {          
          if (object@test@exam == "ACC") {
            dataBlo[, ] <- lapply(dataBlo, RecodeToNA, catToNA)  
          }                
        }  
        if (orderedDat){
          dataBlo[, ] <- lapply(dataBlo[, ], function(x) ordered(x))
        }
        subDat$datos <- dataBlo      
        return(subDat)
    }
    object@datAnalysis <- lapply(object@datAnalysis, recodeLA)
        
    # # conserve rows with less than kOmissionThreshold NR data
    isOmissDel <- kOmissionThreshold < 1 & kOmissionThreshold > 0
    if (isOmissDel) {
      delOmiss <- function(data){
        if (is.null(data$datos)){
          return(NULL)
        } else {
          if (nrow(data$datos) == 0)
             return(NULL)
        }
        misRow  <- rowMeans(data$datos == "")
        isKeep  <- misRow <= kOmissionThreshold
        data$datos    <- data$datos[isKeep, ]
        return(data)       
      }
      object@datAnalysis <- lapply(object@datAnalysis, delOmiss)
    }
    return(object)
})

setGeneric(name = "buildSubsets", 
           def = function(object, flagTotal = TRUE, flagSubCon = FALSE, 
                          flagOri = FALSE, fileExt = NULL, 
                          infoItem = list('sheetExt' = NULL, 
                          'id' = NULL, 'id_Subset' = NULL), ...){standardGeneric("buildSubsets")})
setMethod("buildSubsets", "Analysis", 
  function(object, ...){
    auxDictionary  <- object@test@dictionaryList$variables
    auxParGet      <- ifelse(flagOri, "oriBlock", "calBlock")
	  if(!is.null(fileExt)){
		  if(class(fileExt))
			  stop("ERROR.... O_O Se debe definir la ruta de un archivo de Excel (parametro 'fileExt')")
		  if(!gsub("(.+)\\.(.+)", "\\2", fileExt) %in% c("xlsx", "xls"))
			  stop("ERROR.... O_O Su archivo no es de Excel (parametro 'fileExt')")
		  if(is.null(infoItem$sheetExt))
			  stop("ERROR.... O_O Se debe definir la hoja del archivo (parametro 'infoItem$sheetExt')")
		  if(is.null(infoItem$id) | is.null(infoItem$id_Subset))
			  stop("ERROR.... O_O Se debe elegir las columnas a tomar (parametro 'infoItem')")
		  if(!file.exists(fileExt))	
			  stop("ERROR.... O_O No se encontro el archivo")
		  require(XLConnect)
		  channel   <- XLConnect::loadWorkbook(fileExt)
      auxSheets <- XLConnect::getSheets(channel)
      if (!infoItem$sheetExt %in% auxSheets) {
    	  stop("ERROR.... O_O  Some tables are not found in the file: \n",
         	   fileExt)
  		}

      # # Find max number of columns
      auxNcol <- apply(object@test@datBlock, ncol)
      isMax   <- which(auxNcol == max(auxNcol))[1]
      maxCon  <- names(object@test@datBlock)[isMax]
      cat("--- Se construira los subConjuntos con: ", maxCon, "\n")
      object@datAnalysis <- list()
      if (!flagTotal){
         warning("___Ojo___  No se correra el análisis con todos los ítems del:", maxCon,  
               "\n           Se tendra solo encuenta el archivo", fileExt, "\n", 
               "cambiar el parametro flagTotal")
      }
      filConP <- subset(auxDictionary, codigo_prueba == auxPrueba)
      if (flagTotal){        
        auxPrueba <- gsub("\\.con", "", test)
        object@datAnalysis[[auxPrueba]] <- list(
          'datos' = object@test@datBlock[maxCon][[1]][[auxParGet]],
          'dictionary' = filConP)
      }
      if (flagSubCon){
        warning("___Ojo___ No se tendra encuenta el subCon del diccionario, 
                se construye los bloques con el archivo", fileExt)
      }
	  } else {
		  object@datAnalysis <- list()
		  for(iiTest in names(object@test@datBlock)){
			  auxPrueba <- gsub("\\.con", "", iiTest)
        if(!flagTotal & !flagSubCon){
          stop("___Error___ Se debe definir si se quiere hacer análisis\n", 
               "con Todo los ítems (flagTotal) o por SubConjunto (flagSubCon)")
        }
        filConP <- subset(auxDictionary, codigo_prueba == auxPrueba)
        if(flagTotal) {          
			    object@datAnalysis[[auxPrueba]] <- list(
				    'datos'      = object@test@datBlock[iiTest][[1]][[auxParGet]],
				    'dictionary' = filConP)
        }
        if(flagSubCon) {
          for(jjSubCon in unique(filConP$subCon)){             
             filterDict <- subset(filConP, subCon == jjSubCon)
             colGet     <- names(object@test@datBlock[iiTest][[1]][[auxParGet]])
             colGet     <- c(grep("^([A-Z]|[a-z]){3,}", colGet, value = TRUE), filterDict[, "id"])
             auxKey     <- paste0(auxPrueba, "::", jjSubCon)
             object@datAnalysis[[auxKey]] <- list(
                'datos'      = object@test@datBlock[iiTest][[1]][[auxParGet]][, colGet, with = FALSE],
                'dictionary' = filterDict)

          }
        }
		  }
	  }
    #return(object)
    object <- filterSubsets(object, ...)
    object@datAnalysis <- structure(object@datAnalysis, parameters = match.call(),
                                    date = format(Sys.time(), "%m_%d_%Y %X"))
    return(object)
})


# # Definition of filters
setGeneric(name = "filterAnalysis", def = function(object){standardGeneric("filterAnalysis")})
setMethod("filterAnalysis", "Analysis", 
          function(object){
            auxParam  <- getParams(object)
            allParams <- names(formals(buildSubsets))
            allParams <- union(allParams, names(formals(filterSubsets)))
            allParams <- intersect(allParams, names(auxParam))
            do.call(buildSubsets, c(list(object), auxParam[allParams]))
          })

# # Save result of Analysis
setGeneric(name = "saveResult", def = function(object, ...){standardGeneric("saveResult")})
setMethod("saveResult", "Analysis", function(object, listResults){
     outRdata <- object@outFile$pathRdata
     if (!file.exists(outRdata)){
       save(listResults, file = outRdata)
     } else {
       auxResult <- listResults
       load(file = outRdata)
       for (resul in names(auxResult)){
         listResults[[resul]] <- auxResult[[resul]]
       }     
       save(listResults, file = outRdata)
     }
})


################################################################################
# # Definición Clase FactorClass
################################################################################