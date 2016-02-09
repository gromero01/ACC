################################################################################
# # pruebaClass.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Jorge Mario Carrasco
# #
# # SABER 359, SABER PRO, SABER 11 y ACC
# # Description: Función creada para definar la clase prueba y la clase análisis
# #              para las funciones de anal item
# #
# # Outputs: Funciones para crear objetos y metodos
# #
# # File history:
# #   20150914: Creation
# #   
# # ToDo:
# #       Inclusión de la definición de las clases y los metodos
################################################################################
Analisis <- setClass("Analisis",  					  
 					  # # Definir la estructura
 					  slots = c(
 					  	scripName = 'character',
 					  	param     = 'list', 
 					  	inputFile = 'list',
 					  	outFile   = 'list'), 
 					  
 					  # # Definir los valores por defecto
 					  prototype = list(scripName = "", param = list(), inputFile = list(), outFile = list()), 
 					  validity  = function(object){ 					  	
 					  	if (is.null(object@scripName)) return("Error el Analisis debe tener un scripName")
 					  	if (object@scripName == "00CrearRdata" & length(object@inputFile) == 0) 
 					  		return("Para el paso de CrearRdata se debe tener inputFile")
 					    return(TRUE)	
 					  })

# # Metodos de la clase Análisis
setGeneric(name = "getParams", def = function(object){standardGeneric("getParams")})
setMethod("getParams", "Analisis", function(object){return(object@param)})

setGeneric(name = "getInput", def = function(object){standardGeneric("getInput")})
setMethod("getInput", "Analisis", function(object){return(object@inputFile)})

setGeneric(name = "getOutput", def = function(object){standardGeneric("getOutput")})
setMethod("getOutput", "Analisis", function(object){return(object@outFile)})

setGeneric(name = "leerInsumos", def = function(object){standardGeneric("leerInsumos")})


#setGeneric(name = "correrAnal", def = function(object){standardGeneric("correrAnal")})

Prueba <- setClass("Prueba", 
 					  # # Definir la estructura
 					  slots = c(
 					  	path      = 'character',
 					  	pathDic   = 'character',
 					  	pathRdata = 'character', 
 					  	Analisis  = 'list', 
 					  	exam    = 'character',
						verEntrada = 'numeric',						
						verSalida  = 'numeric', 
						nomPrueba = 'character'
 					  	), 
 					  
 					  # # Definir los valores por defecto
 					  prototype = list(path = "", pathDic = "", pathRdata = "", 
 					  	               Analisis = list(Analisis()), exam = "", 
 					  	               varEntrada = 1, verSalida = 1, nomPrueba = ""), 
 					  validity  = function(object){
 					  	if(object@path == "" | object@exam == "")
 					  	return(TRUE)
 					  })

# # Metodos de la clase Prueba
setGeneric(name = "analizarPru", def = function(object){standardGeneric("analizarPru")})
setGeneric(name = "AA", def = function(object){standardGeneric("AA")})
setGeneric(name = "recogerInfo", def = function(object){standardGeneric("recogerInfo")})

setGeneric(name = "omsAnal", def = function(object, ...){standardGeneric("omsAnal")})
setGeneric(name = "uniAnal", def = function(object, ...){standardGeneric("uniAnal")})
setGeneric(name = "con2Dict", def = function(object, ...){standardGeneric("con2Dict")})
setGeneric(name = "tctAnalysis", def = function(object, ...){standardGeneric("tctAnalysis")})
setGeneric(name = "explorAnalysis", def = function(object, ...){standardGeneric("explorAnalysis")})
setGeneric(name = "confirmatAnalysis", def = function(object, ...){standardGeneric("confirmatAnalysis")})