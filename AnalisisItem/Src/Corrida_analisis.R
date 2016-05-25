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
source(file.path("Function", "pruebaClass.R"))

source("04Exploratorio.R")

prueba0 <- new('Test', path = paste0(pathExam, "\\PBA116"), 
               exam = "SABERPRO", codMod = "07",
               verInput = 1, nomTest = "SABER 11(Sociales y Competencias)", 
               paramLect = list(infoItem   = c('id' = "CODIGO_ITEM", 'subCon' = "COMPETENCIA_NOMBRE", 
               	                'prueba' = "NOMBRE_PRUEBA"), conDirs = "PBA116.con", valMUO = NA,
                                subConInfo = c('path' = pathExpeci, 'nameSheet' =  "FINAL"))) 


ana1 <- new("Analysis", test = prueba0, outFile = list(pathRdata = "prueba.Rdata"))

paso50  <- new('Analisis', prueba = prueba0,
 	           param = list('flagUni' = TRUE, 'flagMultiC' = TRUE, 'flagMultiNC' = TRUE, 'flagBiFac' = TRUE), 
   	           inputFile = list(), outFile = list(pathRdata = "../Output/05Confirmatorio/confirmatAnalysis.Rdata"))

#explorAnalysis(prueba0)
resulExp <- explorAnalysis(prueba0)
resul    <- confirmatAnalysis(prueba0, paso50)