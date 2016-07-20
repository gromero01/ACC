################################################################################
# # corItemPrueba.R
# # R Versions: Version 2.12.0
# #
# # Author(s): Jorge Mario Carrasco Ortiz
# #
# # Process: SABER 3°, 5° y 9°
# # Description: Graficos de disperción entre dificultades y discriminación
# #
# # Inputs: calibracionesAncla.txt y calibracionesPIL.txt
# #
# # Outputs: Graficos de correlaciones entre anclado y no anclado
# #          
# #
# # File history:
# #  20150219: Modificación para corrida 2015 (No se encontro comentario anteriores)
################################################################################
library(DT)
library(data.table)
library(knitr)
#library(rCharts)
library(plyr)
library(ggplot2)
library(googleVis)
library(knitrBootstrap)
library(rmarkdown)
library(WriteXLS)
library(gridExtra)

dirPandoc <- file.path(Sys.getenv("APPDATA"), "..", "Local\\Pandoc") 
Sys.setenv(RSTUDIO_PANDOC = dirPandoc)
source(file.path("Src\\Function\\WrightMapICFES.R"))
source(file.path("Src\\Function\\tablasHtml.R"))
source(file.path("Src\\Function\\gvisUtils.R"))
# # Parametros en consola
###############################################################################

# cat("----- Creando graficas de Correlacion -----\n")
# # # Lectura de parámetros
# args <- commandArgs();
# print(args)
# # #  check if --args used. This avoids a problem with earlier versions of R
# argsPos <- match("--args", args)

# # #  Extracción de parametros
# if(!is.na(argsPos) && length(args) > argsPos){ 
#   inPath   <- args[argsPos+1];  # directorio de input
#   salPath  <- args[argsPos+4];  # Archivo de Salida pagina web
#   pathHist <- args[argsPos+5];

# } else {
#   stop("Error en los parametros")
# }  
#pathHist  <- ".."; inPath <- "."; 
salPath <- "resulHistorico.html"

################################################################################
# # Cargar Datos
################################################################################
load("Output/06IRT/granDatosReporte.Rdata")
load("Output/06IRT/mpITPERS.Rdata")
load("Output/03TCT/outList_TCT.Rdata")
###############################################################################
# # Armado del reporte Web
################################################################################
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
# # Descomprimiendo archivos json
#fileZip <- '\\\\icfesserv5\\academica$\\lib.zip'
#unzip(fileZip, overwrite = TRUE, exdir = ".")
# 
################################################################################
