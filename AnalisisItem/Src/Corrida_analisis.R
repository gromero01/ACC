################################################################################
# # pruebaClass.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Jorge Mario Carrasco
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
# # Global Definitions
################################################################################
inPath  <- file.path("..","Input")
srcPath <- file.path("..","Src")
docPath <- file.path("..","Doc")
funPath <- file.path("Function")
outPath <- file.path("..", "Output")
logPath <- file.path("..", "Log")

options(encoding = "UTF-8")
source(file.path("Function", "pruebaClass.R"))
source("03TCT.R")
source("04Exploratorio.R")
source("06IRT.R") 

################################################################################
# # Execute 
################################################################################

dirPandoc <- file.path(Sys.getenv("APPDATA"), "..", "Local\\Pandoc") 
Sys.setenv(RSTUDIO_PANDOC = dirPandoc)
#Sys.setenv(RSTUDIO_PANDOC = "C:\\Program Files (x86)\\Pandoc")
fileJson  <- "../Input/parameters.json"
listTests <- analyzeTests(fileJson, fUpdate = FALSE)
jointReports(listTests, fileJson, pathJS = "../../../../lib", flagView = FALSE)

publishRepo(fileJson, pathDest = "C:\\Users\\jcarrasco\\Desktop\\Version1", 
            flagActualizar = TRUE)

################################################################################
# # Depuración metodo 
################################################################################
prueba0 <- listTests[[1]]
object  <- IRT(prueba0)