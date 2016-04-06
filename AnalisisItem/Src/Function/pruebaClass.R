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
 					  	outPath   = 'character'), 
 					  
 					  # # Definir los valores por defecto
 					  prototype = list(scripName = "", param = list(), inputFile = list(), outPath = ""), 
 					  validity  = function(object){ 					  	
 					  	if (object@scripName == "") return("Error el Analisis debe tener un scripName")
 					  	if (object@scripName == "00CrearRdata" & length(object@inputFile) == 0) 
 					  		return("Para el paso de CrearRdata se debe tener inputFile")
 					    if (object@outPath == "") 
 					    	return("Error, no hay directorio de salida")
 					    return(TRUE)	
 					  })
# # Metodos de la clase Análisis
setGeneric(name = "getParams", def = function(object){standardGeneric("getParams")})
setMethod("getParams", "Analisis", function(object){return(object@param)})

setGeneric(name = "getInput", def = function(object){standardGeneric("getInput")})
setMethod("getInput", "Analisis", function(object){return(object@inputFile)})

setGeneric(name = "getOutput", def = function(object){standardGeneric("getOutput")})
setMethod("getOutput", "Analisis", function(object){return(object@outPath)})

setGeneric(name = "runSource", def = function(object){standardGeneric("runSource")})
setMethod("runSource", "Analisis", function(object){return(object@outPath)})

#setGeneric(name = "leer", def = function(object){standardGeneric("leer")})
#source("Function/00CrearRdata.R")

aTCT <- setClass("TCT",
					  # # Heredar atributos de la clase Analisis
 					  contains = "Analisis",
 					  # # Definir los valores por defecto
 					  prototype = list(scripName = "03TCT.R", 
 					  				   param     = list(kApli = c(2, 3, 4, 6), 
 					  				   					kOmissionThreshold = 0.8, 
 					  				   					catToNA = c('No Presentado', 'NR', 'Multimarca'),
 					  				   					kCodNElim = '06', 
 					  				   					isCheckKeys = FALSE)),
 					  # # Funciones de validacion
 					  validity  = function(object){ 	
 					    object@outPath = file.path(object@outPath, "03TCT")
 					    print(object)				  	
 					  	if (object@param$kOmissionThreshold == "") 
 					  		return("Error, se necesita el parametro del umbral de omisiones")
 					  	if (object@param$kOmissionThreshold > 1 | object@param$kOmissionThreshold < 0)
 					  		return("Error, el valor del umbral debe estar entre 0 y 1")
 					  })

setMethod ("show", "TCT" , 
  function ( object ){
                cat (" TCT Parameters \n") 
                cat (" Script Name :", object@scripName, "\n") 
                cat (" Omission Threshold :", object@param$kOmissionThreshold , "\n") 
                cat (" Is considered as NA :", object@param$catToNA, "\n") 
                cat (" Reverse key items with negative loadings (?psych::alpha) :", object@param$isCheckKeys, "\n") 
                cat ("\n")
  }) 

# paso01	  <- new('TCT', outPath = "../Output")

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