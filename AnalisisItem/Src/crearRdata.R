library(plyr)
source("Function/pruebaClass.R")
inData <- "../Input"
nomDic <- "diccionario_EstudianteCensal_SABER5_00CC_v00.xlsx"
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
    lisCon[[prueba]] <- lisCon[[prueba]][!grepl("sblqnull|sblq0", lisCon[[prueba]])]
	auxConDir 		 <- gsub("(.+)\\/((PBA|sblq|pba|SBLQ).+\\.con$)",
							 "\\2", lisCon[[prueba]])

	# # 00CrearRdata.R
	paso00    <- new('Analisis', scripName = "00CrearRdata.R",
					  outPath = "../Output",
	 	              param = list(kApli = c(2, 3, 4, 6), nameSheet = "Diccionario00"),
	   	              inputFile = list('Estructura' = file.path(inData, nomDic),
	   	              	               'conDirs'    = sort(auxConDir)))
	# # # 01Omisiones.R
	# paso01	  <- new('Analisis', scripName = "01Omisiones.R",
	# 			   		  param = list(),
	# 			   		  inputFile = list())
	# # # 02Univariados.R
	# paso02	  <- new('Analisis', scripName = "02Univariados.R",
	# 			   		  param = list(),
	# 			   		  inputFile = list())
	# # 03TCT.R
	paso03	  <- new('TCT', outPath = "../Output")
	# # 04Exploratorio.R
	paso04	  <- new('Analisis', scripName = "04Exploratorio.R",
					  			 outPath = "../Output",
	               				 param = list(),
	               				 inputFile = list())
	# # 05Confirmatorio.R
	paso05    <- new('Analisis', scripName = "05Exploratorio.R",
								 outPath = "../Output",
								 param = list('flagUni' = TRUE, 'flagMultiC' = TRUE, 
								 			  'flagMultiNC' = TRUE, 'flagBiFac' = TRUE),
	   	           				 inputFile = list())
	# # 06IRT.R
	prueba0   <- new('Prueba', path = prueba,
	 	        Analisis = list('00CrearRdata.R' = paso00, '03TCT.R' = paso03, 
	 	        				'04Confirmatorio.R' = paso04, '05Confirmatorio.R' = paso05), 
	 	        exam = "ACC", 
	 	        verEntrada = 1, verSalida = 2, 
	 	        nomPrueba = paste0("ACC (", auxConDir, ")"))
	controlData[[prueba]] <- prueba0
}


save(controlData, file = "../Input/controlData.Rdata")
