################################################################################
# # 00CrearRdata.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Alvaro Uzaheta
# #
# # SABER 5 y 9
# # Description: Read dictionary and data for FA study saving Rdata in the
# #               output directory
# #
# # Inputs: Information downloaded from AnalItem for students
# #         questionnaire
# #
# # Outputs: Rdata with dictionary information and Rdata with decode
# #           data for analyses
# #
# # File history:
# #   20140128: Creation
# #   20150914: (Jorge Carrasco) Include the read process of SABER
# #             359, SABER 11 y SABER PRO. Also including parameters
# #             passed by console
# # ToDo:
# #       Include cases when the required topic doesn't have data
# #       Include check for multiples main folders
# #       Include date and name of the dictionary in the attributes if
# #       those files
# #       Review the 'no present' in the ReadData
################################################################################

options(encoding = "UTF-8")


################################################################################
# # Definition of input and output paths
################################################################################
# #  create the outPath, if it exits don't
dir.create(outPath, showWarnings = TRUE)

################################################################################
# # source of scripts with functions
################################################################################
#source(file.path(funPath, "readTranslate.R"))  # # functions to make dictionaries

################################################################################
# # Command line parameters
################################################################################

# # Cargando parametros de las pruebas
load(file.path(inPath, controlFile))
#object <- controlData[[1]]
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
  inPath  <- file.path("..", "Input")
  outPath <- file.path("..", "Output", "00Crear")
  logPath <- file.path("..", "Log")
  funPath <- file.path("Function")
  
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

    # # Save directions of Rdata and datDictionary    
    object@pathDic   <- datDictionary
    object@pathRdata <- datReadBlock
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
	    datBlock <- ReadGeneric(object, dict = dictionaryList, verbose =  FALSE)
	    save(datBlock, file = datReadBlock)
    }
    return(object)
  })

################################################################################
# # Apply the read function to each test in controlData
################################################################################
for (prueba in names(controlData)) {
	# prueba0 <- leerInsumos(prueba0)
  controlData[[prueba]] <- leerInsumos(controlData[[prueba]])
}

save(controlData, file = file.path(inPath, controlFile))

################################################################################
# # Save the Population description for eac h Test (SABER 3, 5, 9)
################################################################################
finalCount <- NULL
for (prueba in names(controlData)) {
   if (controlData[[prueba]]@exam == "SABER359"){
     load(controlData[[prueba]]@pathRdata)
     for (subPrueba in names(datBlock)) {
       auxPrueba  <- subset(datBlock[[subPrueba]]$calBlock, select = c("SNP", "Grado", "Estado_Final"))
       conteoP    <- auxPrueba[, as.data.frame(table(Grado, Estado_Final))]
       conteoP    <- cbind(conteoP, 'subPrueba' = gsub("\\.con", "", subPrueba),
                           'prueba' = prueba)
       conteoP$Estado_Final  <- mapvalues(conteoP$Estado_Final, from = c("1", "2", "3", "4", "5", "6"),
                               to = c("Censal", "Control", "SobreMuestra", "Espcial",
                                      "Adicional Censal", "Adicional Control"))
       finalCount <- rbind(finalCount, conteoP)
     }
   }
}
save(finalCount, file = "countTipoApli.Rdata")


