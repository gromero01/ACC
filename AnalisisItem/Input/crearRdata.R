setwd("../Src")
source("Function/pruebaClass.R")
source("00CrearRdata.R")
pathExpeci <- "../Input/Especificaciones.xlsx"
pathExam   <- "EK20143\\exam657"
prueba0 <- new('Prueba', path = paste0(pathExam, "\\pbaF200501"), 
               exam = "SABERPRO", 
               verEntrada = 1, verSalida = 1, nomPrueba = "SABER PRO(Comunicación Juridica)", 
               paramLect = list(infoItem   = c('id' = "CODIGO_ITEM", 'indice' = "AFIRMACIÓN_V", 'prueba' = "CODIGO_ICFES_PRUEBA_FORMA"),
                                conDirs    = "pbaF200501.con", 
                                indiceInfo = c('path' = pathExpeci, 'nameSheet' =  "FINAL")))