################################################################################
# # pruebaClass.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Jorge Mario Carrasco
# #
# # SABER 359, SABER PRO, SABER 11 y ACC
# # Description: Función creada para definar la clase prueba, la clase análisis y
# #              reporte prueba para las definir las funciones de análisis de item.
# #
# # Outputs: Funciones para crear objetos y metodos
# #
# # File history:
# #   20150914: Creation
# #   
# # ToDo:
# #       Inclusión de la definición de las clases y los metodos
################################################################################

################################################################################
# # Definición clase Prueba
################################################################################

Prueba <- setClass("Prueba", 
 		# # Definir la estructura
 		slots = c(path      = 'character',
 			      pathDic   = 'character',
 			      pathRdata = 'character',  					  	
 			      exam      = 'character',
	              verEntrada = 'numeric',						
	              verSalida  = 'numeric', 
	              nomPrueba = 'character',
	              paramLect = 'list'),  					  
 		# # Definir los valores por defecto
 		prototype = list(varEntrada = 1, verSalida = 1,  					  	 
					  	 nomPrueba = "", paramLect = list(conDirs = "")), 
 		validity  = function(object){ 			
 			if(object@path == "" | object@exam == "")
 				stop("Se debe definir el directorio del examen")
 			if(object@paramLect$conDirs == "")
 				stop("El parametro 'conDirs' debe contener almenos un .con")
 			return(TRUE)
 		})

# # Metodos de la clase Prueba
setGeneric(name = "leerInsumos", def = function(object){standardGeneric("leerInsumos")})
setMethod("initialize", "Prueba", function(.Object, ...) {    
    .Object <- callNextMethod()    
    if (length(.Object@path) != 0){
    	if (length(.Object@pathDic) == 0 & length(.Object@pathRdata) == 0){
          .Object <- leerInsumos(.Object)
    	}    	
    }
    .Object
  })
################################################################################
# # Definición Clase Analisis
################################################################################

Analisis <- setClass("Analisis",  					  
 					 # # Definir la estructura 					  	                 
	                 contains = 'Prueba', 
 					 slots = c(
 					  	prueba = 'Prueba', 					  	
 					  	param     = 'list', 
 					  	inputFile = 'list',
 					  	outFile   = 'list'),  					  
 					 # # Definir los valores por defecto
 					 prototype = list(prueba = NULL, scripName = "", param = list(), 
 					                 inputFile = list(), outFile = list(pathRdata = "")), 
 					 validity  = function(object){ 					  	 					  	
 					  	if (object@outFile$pathRdata == "")
 					  		return("Se debe definir un directorio de salida de resultados (Rdata)")
 					    return(TRUE)	
 					  })

# # Metodos de la clase Análisis
setGeneric(name = "getParams", def = function(object){standardGeneric("getParams")})
setMethod("getParams", "Analisis", function(object){return(object@param)})

setGeneric(name = "getInput", def = function(object){standardGeneric("getInput")})
setMethod("getInput", "Analisis", function(object){return(object@inputFile)})

setGeneric(name = "getOutput", def = function(object){standardGeneric("getOutput")})
setMethod("getOutput", "Analisis", function(object){return(object@outFile)})

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

