library(plyr)
source("../Src/Function/pruebaClass.R")
inData <- "."
lisCon <- list.files(inData, ".con$", recursive = TRUE, full.names = TRUE)
lisCon <- split(lisCon, f = gsub("(.+)\\/(PBA|sblq|pba|SBLQ).+\\.con$", "\\1", lisCon))

controlData <- list()
for (prueba in names(lisCon)){
	#grado  <- gsub(".+SABER(\\d)\\/.+", "\\1", prueba)
  	#letaAr <- unique(gsub(".+(pba|sblq|PBA|SBLQ)(\\w).+", "\\2", lisCon[[prueba]]))
	#letaAr <- mapvalues(letaAr, from = c("L", "M", "C", "D", "F"),
        #                to = c("Lenguaje", "Matemáticas", "Ciencias", "Competencias Ciudadanas",
        #                       "Financiera"), warn_missing = FALSE)

        # #Excluyendo SBLnull
        lisCon[[prueba]] <- lisCon[[prueba]][!grepl("sblqnull", lisCon[[prueba]])]
	auxConDir <- gsub("(.+)\\/((PBA|sblq|pba|SBLQ).+\\.con$)",
		              "\\2", lisCon[[prueba]])

	paso00    <- new('Analisis', scripName = "00CrearRdata.R",
	 	              param = list(kApli = c(2, 3, 4, 6), nameSheet = "Diccionario00"),),
	   	              inputFile = list('Estructura' = NULL,
	   	              	               'conDirs'    = sort(auxConDir)))
	paso40  <- new('Analisis', scripName = "04Exploratorio.R",
	               param = list(
	               inputFile = list())
	paso50  <- new('Analisis', scripName = "05Exploratorio.R",
	 	             param = list('flagUni' = TRUE, 'flagMultiC' = TRUE, 'flagMultiNC' = TRUE, 'flagBiFac' = TRUE),
	   	           inputFile = list())

	prueba0 <- new('Prueba', path = prueba,
	 	        Analisis = list('00CrearRdata.R' = paso00, '04Confirmatorio.R' = paso40,
	 	           	'05Confirmatorio.R' = paso50), exam =
                        "SABERPRO",
	  	        verEntrada = 1, verSalida = 1,
	  	        nomPrueba = paste0("SABER_PRO (", auxConDir, ")"))
	controlData[[prueba]] <- prueba0
}


save(controlData, file = "controlData.Rdata")
