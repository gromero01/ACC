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
source(file.path(funPath, "tablasHtml.R"))
source(file.path(funPath, "pruebaClass.R"))
source("00Filtros.R")
source("03TCT.R")
source("04Exploratorio.R")
source("06IRT.R") 

################################################################################
# # Execute 
################################################################################

dirPandoc <- file.path(Sys.getenv("APPDATA"), "..", "Local\\Pandoc") 
Sys.setenv(RSTUDIO_PANDOC = dirPandoc)
#Sys.setenv(RSTUDIO_PANDOC = "C:\\Program Files (x86)\\Pandoc")

#fileJson  <- c("../Input/parameters_EN.json", "../Input/parameters_MA.json", 
#	           "../Input/parameters_PR.json")

vecJson  <- c("../Input/parameters_EN.json", "../Input/parameters_MA.json", 
              "../Input/parameters_PR.json","../Input/parameters_CC.json",
              "../Input/parameters_RC.json","../Input/parameters_LC.json")

for (fileJson in vecJson){
  listTests <- analyzeTests(fileJson)
  jointReports(listTests, fileJson, pathJS = "../../../../lib", flagView = FALSE)
}

publishRepo(vecJson, pathDest = "C:\\Users\\jcarrasco\\Desktop\\Version1", 
            flagActualizar = FALSE)



################################################################################
# # Depuración metodo 
################################################################################
#prueba0 <- listTests[[1]]
#object  <- listTests[[1]]@listAnal[["IRT"]]
#codeAnalysis(object)