################################################################################
# # pruebaClass.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s):
# #
# # SABER 11
# # Description: Función creada para definar la clase prueba, la clase análisis y
# #              reporte prueba para las definir las funciones de análisis de item.
# #
# # Outputs: Funciones para crear objetos y metodos
# #
# # File history:
# #   20160302: Creation
# #   
# # ToDo:
# #       
################################################################################

################################################################################
# # global paths 
################################################################################
# # docPath <- file.path("..","doc","latex")
inPath  <- file.path("..","Input")
srcPath <- file.path("..","Src")
funPath <- file.path("Function")
outPath <- file.path("..", "Output")
logPath <- file.path("..", "Log")
################################################################################

pathExpeci <- file.path(inPath, "Especificaciones1.xlsx")
pathExam   <- "AC20152"
fileJson <- "../Input/parameters.json"

analyzeTests <- function(fileJson){
  # # inicializa la clase Test para cada prueba en la entrada
  # #
  # # Arg:
  # #  fileJson: [character] la ruta del archivo de parametros
  # #
  # # Ret:
  # #  listTests: [list-Test] lista con todas las pruebas analizadas

  # # Load  scripts
  source(file.path("Function", "pruebaClass.R"))
  source("03TCT.R")
  source("04Exploratorio.R")
  source("06IRT.R")	
  readJson <- fromJSON(fileJson, simplifyVector = TRUE, 
                       simplifyDataFrame = FALSE, 
                       simplifyMatrix = FALSE)
  listTests <- list() 
  # # Comprobando si existe nombre para la salida
  if (! "labelHtml" %in% names(readJson)){
  	stop('___ERROR___ Se debe definir en el archivo de parametros "labelHtml"')
  }
  readJson <- readJson[names(readJson) != "labelHtml"]  	

  for (test in names(readJson)){
    jsonLec   <- readJson[[test]]$paramLect
  	paramLect <- list(infoItem = jsonLec$infoItem, conDirs = jsonLec$conDirs, 
  		              valMUO = jsonLec$valMUO, subConInfo = jsonLec$subConInfo)
    auxTest <- new('Test', path = jsonLec$path, exam = jsonLec$exam, 
    	             codMod = jsonLec$codMod, verInput = jsonLec$verInput, 
    	             nomTest = jsonLec$nomTest, paramLect = paramLect) 
  	auxTest <- runAnalysis(auxTest, jsonTest = readJson[[test]]$Analisis)
  	listTests[[test]] <- auxTest
  }
  return(listTests)
}
  
setGeneric(name = "runAnalysis", def = function(object, ...){standardGeneric("runAnalysis")})
setMethod("runAnalysis", "Test",
function(object, jsonTest){
  # # Ejecuta los analisis con los parametros del archivo .json
  # #
  # # Arg:
  # #  jsonTest: [list] filtro del archivo con los parametros de la prueba
  # #
  # # Ret:
  # #  object: [Analysis] objecto prueba guardando todos los analisis
  for(ii in 1:length(jsonTest)){
     # # Verificando si codeAnalysis existe para ese analisis
     analisis  <- names(jsonTest[[ii]])
     posAnalys <- as.character(methods("codeAnalysis"))
     posAnalys <- gsub("codeAnalysis,(.+)-.+", "\\1", posAnalys)
     if (!analisis %in% posAnalys) {
     	warning('_OjO_______________________________________________________________\n', 
     		    '(El análisis "', analisis, '" no tine clase y/o metodos definidos)\n',
     		    '___________________________________________________________________')
     } else {
       # # Definiendo analisis
       exprAnalysis <- paste0(analisis, "(test = object, paramExp = jsonTest[[ii]][[1]])")
	   auxAnalysis  <- eval(parse(text = exprAnalysis))
	   object@listAnal <- c(object@listAnal, auxAnalysis)
	   names(object@listAnal)[length(object@listAnal)] <- analisis
	   # # Corriendo analisis
	   codeAnalysis(auxAnalysis)
     }
  }
  return(object)
})

setGeneric(name = "buildReport", def = function(object, ...){standardGeneric("buildReport")})
setMethod("buildReport", "Test",
function(object, flagXLSX = TRUE){
  # # Construye todos los reportes de todos los analisis de una prueba
  # #
  # # Arg:
  # #  object: [Analysis] objecto prueba despues de correr runAnalysis
  # #
  # # Ret:
  # #  reportes en html construidos con jQuery y HTML
	for (analysis in object@listAnal){
		if(flagXLSX){
			outXLSX(analysis)
		}
		outHTML(analysis)
	}
})

jointReports <- function(listTests, fileJson){
  # # Crear el reporte html para 1 o varias pruebas
  # #
  # # Arg:
  # #  listTests: [list-Test] lista con todas las pruebas analizadas
  # #
  # # Ret:
  # #  archivos html con el nombre asignado en json$labelHtml
  readJson  <- fromJSON(fileJson, simplifyVector = TRUE, 
                       simplifyDataFrame = FALSE, 
                       simplifyMatrix = FALSE)
  labelHtml <- readJson$labelHtml

  auxDir <- "Doc"
  if (!file.exists(auxDir)) dir.create(auxDir)
  rmarkdown::render('Src\\Sweave\\reportAnaItem.Rmd', 'knitrBootstrap::bootstrap_document', 
  	                output_file = salPath,  output_dir = auxDir, 
                    encoding = "utf-8")
  salPathDoc <- file.path("Doc", salPath)
  # # Incluyendo archivos .js necesarios para las tablas
  jsonLib <- c('  <script src="lib/htmlwidgets-0.5.2/htmlwidgets.js"></script>',
               '  <script src="lib/jquery-1.11.1/jquery.min.js"></script>',
               '  <script src="lib/datatables-binding-0.1.40/datatables.js"></script>',
               '  <script src="lib/datatables-1.10.7/jquery.dataTables.min.js"></script>',
               '  <script src="lib/popUPGR.js"></script>',
  #             '  <script src="lib/FancyZoom_1.1/js-global/FancyZoom.js" type="text/javascript"></script>',
  #             '  <script src="lib/FancyZoom_1.1/js-global/FancyZoomHTML.js" type="text/javascript"></script>',
               '  <link href="lib/datatables-default-1.10.7/dataTables.extra.css" rel="stylesheet" />',
               '  <link href="lib/datatables-default-1.10.7/jquery.dataTables.min.css" rel="stylesheet" />')
               
  archHtml  <- readLines(salPathDoc)
  archCSS   <- grep("<!-- jQuery -->", archHtml)
  indCSStab <- grep("/\\* style tables \\*/", archHtml)
  archHtml  <- gsub("font-weight: bold;", "font-weight: bold; \n color: red; \n background-image: url(\"FondoPresentacionesICFES01.png\");" , archHtml)
  archHtml  <- gsub("<body>", "<body onload=\"setupZoom()\">" , archHtml)  
  #archHtml  <- archHtml[-seq(indCSStab, indCSStab + 1)]
  archHtml[indCSStab + 1]  <- "$('table').addClass('table-bordered table-condensed');"
  
  archHtml <- c(archHtml[1:(archCSS + 2)], jsonLib, archHtml[(archCSS + 3):length(archHtml)])
  cat(archHtml, file = salPathDoc, sep = "\n")
  
  openHTML <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))
  openHTML(salPathDoc)
}

