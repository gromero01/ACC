################################################################################
# # pruebaClass.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Jorge Mario Carrasco, Nelson Rodriguez, William Acero, 
# #            Robert Romero
# #
# # SABER 359, SABER PRO, SABER 11 y ACC
# # Description: Función creada para definar la clase prueba, la clase análisis y
# #              reporte prueba para las definir las funciones de análisis de item.
# #
# # Outputs: Funciones para crear objetos y metodos
# #
# # File history:
# #   20150914: Creation
# #   20160516: Modificación de las clases prueba y Análisis
# # ToDo:
# #       Inclusión de la definición de las clases y los metodos
################################################################################

################################################################################
# # Definición clase Prueba
################################################################################
Prueba <- setClass("Prueba", 
 		# # Definir la estructura
 		slots = c(path           = 'character', # Ruta de la carpeta 
 			      dictionaryList = 'list',		# Diccionario de variables
 			      datBlock       = 'list',  	# 
 			      exam           = 'character',
	              verEntrada     = 'numeric',
	              nomPrueba      = 'character',
	              paramLect      = 'list', 
	              codMod         = 'character'),  

 		# # Definir los valores por defecto
 		prototype = list(verEntrada = 1, path = "", exam = "", codMod = "02",
					  	 nomPrueba = "", paramLect = list(conDirs = "")), 
 		validity  = function(object){ 			 			
 			auxModelos <- data.frame('codMod' = c("00", "01", "02", "03", "04", "05", "06", "07"),
                                     'model' = c("noModel", "PCM", "RSM", "GRM", "MRD", "2PL", "1PL", "3PL"))				
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

# # Metodos de la clase Prueba
setGeneric(name = "leerInsumos", def = function(object){standardGeneric("leerInsumos")})

setMethod("leerInsumos", "Prueba",
function(object){
  ################################################################################
  # # Libraries
  ################################################################################
  require(XLConnect)
  require(LaF)  # # 0.5, Fast access to large ASCII files
  require(plyr)
  require(data.table)  # # 1.8.10, fast indexing, ordered joint, ...
  
  ################################################################################
  # # Definition of input and output paths
  ################################################################################
  outPath <- file.path("..", "Output", "00Crear")
  if (dir.exists(outPath)) {
  	unlink(outPath, recursive = TRUE)
  }
  dir.create(outPath, showWarnings = TRUE)
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
    if (is.na(controlPrueba@verEntrada))
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
                                 controlPrueba@verEntrada, ".RData", sep = ""))
      datReadBlock  <- file.path(outPath, 
                           paste("datBlock_V",
                                 controlPrueba@verEntrada, ".RData", sep = ""))
    } else {
      datDictionary <- file.path(outPath,
                               paste("dictionaryList_",
                                     gsub(".+(\\\\|\\/)(.+)$", "\\2", controlPrueba@path), "_V",
                                     controlPrueba@verEntrada, ".RData", sep = ""))
      datReadBlock  <- file.path(outPath,
                               paste("datBlock_",
                                     gsub(".+(\\\\|\\/)(.+)$", "\\2", controlPrueba@path),"_V",
                                     controlPrueba@verEntrada, ".RData", sep = ""))
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
        save(dictionaryList, file = datDictionary)

      } else {
      	load(datDictionary)
      }
      # # Reading the DB using generic dictionary (only for dichotomous items)
      if (is.null(object@paramLect$valMUO))
        object@paramLect$valMUO <- 9
	  datBlock <- ReadGeneric(object, dict = dictionaryList, verbose =  FALSE, 
                              valMUO = object@paramLect$valMUO)
	  save(datBlock, file = datReadBlock)
    }
    
    # # Save directions of Rdata and datDictionary    
    object@dictionaryList <- dictionaryList
    object@datBlock       <- datBlock

    return(object)
  })


setMethod("initialize", "Prueba", function(.Object, ..., prueba) {    
    .Object <- callNextMethod()
    if(missing(prueba)){
    	if (length(.Object@path) != 0){
    		if (length(.Object@dictionaryList) == 0 & length(.Object@datBlock) == 0){
          		.Object <- leerInsumos(.Object)
    		}    	
    	}
    }
    .Object
  })
################################################################################
# # Definición Clase Analisis
################################################################################

Analisis <- setClass("Analisis",  					  
 					 # # Definir la estructura                
 					 slots = c(
 					  	prueba         = 'Prueba',
 					  	datAnalisis = 'list',
 					  	param       = 'list', 
 					  	inputFile   = 'list',
 					  	outFile     = 'list', 
 					  	verSalida   = 'numeric'),  					  
 					 # # Definir los valores por defecto
 					 prototype = list(prueba = NULL, scripName = "", param = list(), 
 					                 inputFile = list(), outFile = list(pathRdata = "")), 
 					 validity  = function(object){ 					  	 					  	
 					  	if (object@outFile$pathRdata == "")
 					  		return("Se debe definir un directorio de salida de resultados (Rdata)")
 					    return(TRUE)	
 					  })

setMethod("initialize", "Analisis", function(.Object, ..., prueba) {
	.Object <- callNextMethod()
    if(missing(prueba)){
      stop("Se debe especificar un objeto 'prueba = '??? ")
    }
    .Object@prueba <- prueba
    .Object
  })

# # Metodos de la clase Análisis
setGeneric(name = "getParams", def = function(object){standardGeneric("getParams")})
setMethod("getParams", "Analisis", function(object){return(object@param)})

setGeneric(name = "getInput", def = function(object){standardGeneric("getInput")})
setMethod("getInput", "Analisis", function(object){return(object@inputFile)})

setGeneric(name = "getOutput", def = function(object){standardGeneric("getOutput")})
setMethod("getOutput", "Analisis", function(object){return(object@outFile)})

setGeneric(name = "buildSubsets", def = function(object, ...){standardGeneric("buildSubsets")})
setMethod("buildSubsets", "Analisis", 
  function(object, fExtern = FALSE, fileExt = NULL, sheetExt = NULL, 
  	       infoItem = list('id' = NULL, 'id_Subset' = NULL)){
	if(fExtern){
		if(is.null(fileExt))
			stop("ERROR.... O_O Se debe definir un archivo de Excel (parametro 'fileExt')")
		if(!gsub("(.+)\\.(.+)", "\\2", fileExt) %in% c("xlsx", "xls"))
			stop("ERROR.... O_O Su archivo no es de Excel (parametro 'fileExt')")
		if(is.null(sheetExt))
			stop("ERROR.... O_O Se debe definir la hoja del archivo (parametro 'sheetExt')")
		if(is.null(infoItem$id) | is.null(infoItem$id_Subset))
			stop("ERROR.... O_O Se debe elegir las columnas a tomar (parametro 'infoItem')")
		if(!file.exists(fileExt))	
			stop("ERROR.... O_O No se encontro el archivo")
		require(XLConnect)
		channel   <- XLConnect::loadWorkbook(fileExt)
      	auxSheets <- XLConnect::getSheets(channel)
      	if (! sheetExt %in% auxSheets) {
    		stop("ERROR.... O_O  Some tables are not found in the file: \n",
         	     fileExt)
  		}
	} else {
		object@datAnalisis   <- list()
		auxDictionary <- object@prueba@dictionaryList$variables
		for(prueba in names(object@prueba@datBlock)){
			auxPrueba <- gsub("\\.con", "", prueba)
			object@datAnalisis[auxPrueba] <- list(
				'datos'      = object@prueba@datBlock[[prueba]]
				'dictionary' = subset(auxDictionary, codigo_prueba == auxPrueba))
		}		
	}
  })


setGeneric(name = "loadDatB", def = function(object, ...){standardGeneric("loadDatB")})

setGeneric(name = "loadDatB", def = function(object, ...){standardGeneric("loadDatB")})
setMethod("loadDatB", "Analisis", 
	function(object, recodeNA = TRUE, catToNA = NULL, kOmissionThreshold = 1, ...){
      # # Load datBlock and dictionaryList	  
      if (file.exists(object@prueba@pathRdata)){
        load(object@prueba@pathRdata)	
      } else {
        stop("Error en la clase PRUEBA, no se encontro la ruta (pathRdata)")
  	  }
  	  
	  # # conserved data from sample application
	  if(object@exam == "ACC"){
		  if(isTypeB){
		    if (!is.data.frame(datBlock) & is.list(datBlock) & length(datBlock) == 1) {
		      datBlockControl <- list(subset(datBlock[[1]], x$tipoApli %in% object@param$kApli))
		      names(datBlockControl) <- names(datBlock)
		    } else {
		      datBlockControl <- lapply(datBlock, function(x)
		                                subset(x, x$tipoApli %in% object@param$kApli))
		    }
		  } else{
			  datBlockControl <- datBlock
		  }
	  } else {
    	  datBlockControl <- lapply(datBlock, function(x) {
        	                        aux <- data.frame(x$calBlock)
            	                    names(aux) <- gsub("X(\\d+)", "\\1", names(aux))
                	                return(aux)})
  	  }

  	  # # Recode 'Multimarca' 'No aplica' 'NR' for NA
  	  
      recodeLA <- function(data){      	  
      	varId  <- intersect(loadDictionary(object, ...)$id, names(data)) 
      	if (length(varId) == 0){
          return(NULL)
      	}
      	dataBlo <- data.table(data[, varId])
      	if (recodeNA) {
      	   if (object@prueba@exam == "ACC") {
      	     if (is.null(catToNA)){
      	  	   stop("Se debe especificar las categorias que no son opciones de respuesta")
      	     }
             dataBlo[, ] <- lapply(dataBlo, RecodeToNA, catToNA)	
      	   } else {
             dataBlo[, ] <- lapply(dataBlo[, ], function(x) ordered(x))
             if (object@exam == "SABERPRO") {
               dataBlo[, ] <- rename(dataBlo[, ], c(SNP = "consLect"))
             }
      	   }
      	}
      	return(dataBlo)
      }
      datBlockControl <- lapply(datBlockControl, recodeLA)
      
      # # conserve rows with less than kOmissionThreshold NR data
      isOmissDel <- kOmissionThreshold < 1 & kOmissionThreshold > 0
      if (isOmissDel) {
        delOmiss <- function(data){
        	if (is.null(data)){
        	  return(NULL)
        	} else {
        	  if (nrow(data) == 0)
        	  	return(NULL)
        	}
      	    if (object@prueba@exam == "ACC") {
      	      misRow  <- rowMeans(data == "")
      	    } else {
      	      misRow  <- rowMeans(data == "")
      	    }
	  		isKeep  <- misRow <= kOmissionThreshold
	  		data    <- data[isKeep, ]
	  		return(data[isKeep , ])	  		
	    }
		datBlockControl <- lapply(datBlockControl, delOmiss)
      }
  	  return(datBlockControl)
    })

setGeneric(name = "loadDictionary", def = function(object, ...){standardGeneric("loadDictionary")})
setMethod("loadDictionary", "Analisis", 
	function(object, kCodNElim = "06", kCodPar = NULL, kCodMod = NULL,
		       flagNI = FALSE, filPrueba = NULL , filIndex = NULL){
      # # Load dictionaryList
      if (file.exists(object@prueba@pathDic)){
        load(object@prueba@pathDic)
      } else {
        stop("Error en la clase PRUEBA, no se encontro la ruta (pathDic)")
  	  }

	  if(!"paralelo" %in% names(dictionaryList$variables)){
	    dictionaryList$variables$paralelo <- "01"
	    save(dictionaryList, file = object@prueba@pathDic)
      }
	  
	  if(!"indice" %in% names(dictionaryList$variables)){
	    dictionaryList$variables$indice <- dictionaryList$variables$codigo_prueba
	    save(dictionaryList, file = object@prueba@pathDic)
      }

	  # # Filtering items
      isNoElim   <- dictionaryList$variables[, 'elimina' ] == kCodNElim
      
      if (!is.null(kCodMod)){
      	isCodMod <- dictionaryList$variables[, 'codMod'] == kCodMod	    
      } else {
      	isCodMod <- rep(TRUE, nrow(dictionaryList$variables))
      }     

      if (!is.null(kCodPar)){
      	isCodPar <- dictionaryList$variables[, 'paralelo'] == kCodPar	    
      } else {
      	isCodPar <- rep(TRUE, nrow(dictionaryList$variables))
      }     

      if (object@prueba@exam == "ACC") {
        if (flagNI){
          isNI     <- dictionaryList$variables[, 'indice'] != 'NI' 
	      isPrueba <- isCodMod & isNoElim & isCodPar & isNI
        }
      } else {
        isPrueba   <- isCodMod & isNoElim & isCodPar
      }     
      # # Filtering items by 'codigo_prueba' and 'indice'
      if (!is.null(filPrueba)) {
        flagPru  <- dictionaryList$variables[, 'codigo_prueba'] %in% filPrueba        
      	isPrueba <- isPrueba & flagPru
      }
      if (!is.null(filIndex)) {
        flagIndex <- dictionaryList$variables[, 'indice'] %in% filIndex
      	isPrueba  <- isPrueba & flagIndex
      }
	  return(dictionaryList$variables[isPrueba, ])
    })

setGeneric(name = "getIndex", def = function(object, ...){standardGeneric("getIndex")})
setMethod("getIndex", "Analisis", 
	function(object, tipo = c("codigo_prueba", "indice", "Ambos")[1], ...){     
      # # Load datBlock and dictionaryList		
	  dictionaryList  <- loadDictionary(object)
	  if (tipo %in% c("Ambos", "indice")) {
	  	splitId <- apply(dictionaryList[, c("codigo_prueba", "indice")], 
	  		             1 , paste , collapse = "::")	
	  } else {
	  	splitId <- dictionaryList[, tipo]
	  }
      
      if (tipo == "Ambos") {
      	splitTest      <- dictionaryList[, "codigo_prueba"]
      	dictionaryList <- c(split(dictionaryList, f = splitTest), 
      		                split(dictionaryList, f = splitId))
      } else {
	    dictionaryList <- split(dictionaryList, f = splitId)
      }

      newIndex <- lapply(dictionaryList, function(x){
      	datBlock <- loadDatB(object, filPrueba = unique(x$codigo_prueba), 
      		                 filIndex = unique(x$indice), ...)
      	datBlock <- data.table::rbindlist(datBlock)
        return(list(dictKk = x, kkBlock = datBlock))
      })
      return(newIndex)
    })

#setGeneric(name = "correrAnal", def = function(object){standardGeneric("correrAnal")})

setMethod("getIndex", "missing", function(x, ...) {
  getIndex()
})
################################################################################
# # Definición Clase ReportePrueba
################################################################################

setGeneric(name = "analizarPru", def = function(object){standardGeneric("analizarPru")})
setGeneric(name = "AA", def = function(object){standardGeneric("AA")})
setGeneric(name = "recogerInfo", def = function(object){standardGeneric("recogerInfo")})

