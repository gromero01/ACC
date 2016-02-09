################################################################################
# # RunDimensionalityCC.R
# # R Versions: 2.15.2
# #
# # Author(s): Jorge Mario Carrasco Ortiz
# #
# # SABER 5° y 9° Citizenship competencies 
# # Description: Generación de prototipos a nivel de escala e item para
# # la prueba de  competencias ciudadanas  
# #             
# #
# #
# # output:  Prototitos para el reporte de resultados de la prueba de
# # competencias ciudadanas SABER 5° y 9°  2012
# #
# #
# # File history:
# #   20130410: Creation
################################################################################
srcPath   <- file.path("..", "src")
funPath   <- file.path("Function")
logPath   <- file.path("..", "log")
rnwPath   <- file.path("Sweave")
inPath    <- file.path("..", "input")
basesPath <- "../../../BASES"
consoPath <- "../../../CONSOLIDADA_MARIO_CC"
evaluPath <- "../../../CONSOLIDADA_MARIO_CC/consolidado"
################################################################################
# # Global options
################################################################################
options(encoding = "UTF-8")

options(stringsAsFactors = FALSE)

################################################################################
# # Load libraries and Run Sources
################################################################################
library(LaF)
library(RODBC)
library(data.table)
library(car)
library(plyr)
source(file.path(funPath, "wrapPS.R"))
source(file.path(funPath, "partirComas.R"))
source(file.path(funPath, "functionsFT.R"))

################################################################################
# # Load databases
################################################################################
# # Lectura codificación de params
tableParamTipo <- leer_LaF(basesPath, "codifcaParam.txt")

# # Lectura de EstabaPequeno
apagaSejos <- leer_LaF(basesPath, "EstabPequenioCC.txt")
indEscala  <- apagaSejos[, paste(grado, cod_escala, sep = "")]
apagaSejos <- split(apagaSejos, f = indEscala)

# # Lectura Niveles de desempeño
nivelesDes <- leer_LaF(consoPath, "niveles.txt")

# # Lectura Sejos Apagadas
#sejosApgar  <- leer_LaF(basesPath, "difbrinfosejos.txt")

# # Lectura codificación de indices
codificaEscala <- leer_LaF("../../../CONSOLIDADA_MARIO_CC", 
                           "codificaIndices2012.txt")

# # Lectura ficha tecnica para establecimientos
infoEstab   <- leer_LaF(basesPath, "infoEstab.txt")
infoEstab   <- infoEstab[, list(daneEstab, enteId, sector, 
                                zonaEstab, nse)] 
setnames(infoEstab, names(infoEstab), c("id_institucion", "ente_Id",
                                        "sector", "zonaEstab", "nivel"))

# # Dividiendo archivos de evaluación por escalas
dirSalida <- dir(evaluPath, full.names = TRUE)
musumSali <- dirSalida[grep(".+\\/constantes-.+", dirSalida)]
dirSalida <- dirSalida[grep(".+\\/Evaluacion-Calificadu.+", dirSalida)]
patrBusq  <- gsub(".+\\/.+-(\\d)-(\\w+)\\d?-INDICE\\.txt", 
                  "\\1\\2", dirSalida)
patrBusq  <- gsub("(\\d\\w+)(IT)+\\d\\d", "\\1", patrBusq)
dirSalida <- split(dirSalida, patrBusq)


# # Lectura ficha tecnica para establecimientos
ftEstab     <- leer_LaF(basesPath, "fichaTecnicaEstab.txt")
indUnicidad <- ftEstab[, paste(id_institucion, id_sede, grado, 
                               cod_escala, sep = "")]
if (length(unique(indUnicidad)) - length(indUnicidad) != 0) {
  stop("Existen duplicados en la ficha tecnica de establecimiento")
}
ftEstab   <- split(ftEstab, f = ftEstab[, paste(grado, cod_escala, sep = "")])


# # Encontrando Nombres de las escalas normales y especiales
nomEscaEsp <- codificaEscala[(grado5 != 0 | grado9 != 0) & cod_item != "NULL", 
                             NOMBRE]
nomEscaEsp <- unique(gsub("^(\\w+)IT\\d+$", "\\1", nomEscaEsp))
nomEscaNor <- codificaEscala[(grado5 != 0 | grado9 != 0) & cod_item == "NULL", 
                             NOMBRE]
auxTabla   <- NULL

# # Dividiendo archivos de Consolidados
nameConsoli <- dir("../../../20122_CC_CALIFICACION/", full.names = TRUE)
nameConsoli <- nameConsoli[grep(".+\\w\\.\\w+\\.\\d\\.desi\\.",
                                nameConsoli)]
patrBusq    <- gsub(".+INDICE\\.(\\w+)\\.(\\d).+\\.txt", "\\2\\1", nameConsoli)
patrBusq    <- gsub("(\\d\\w+)(IT)+\\d\\d", "\\1", patrBusq)
nameConsoli <- split(nameConsoli, f = patrBusq)

# # Media y desviación a la que se quiere llevar
mediaNacional <- 150
varNacional   <- 100
dirFilMulSum  <- file.path(consoPath, "PtosCorteEscala.txt")
varIniciales  <- ls()

if (file.exists(dirFilMulSum)) {
  oldMulSum <- leer_LaF(nomDirec = "./", fileBase = dirFilMulSum)
  oldMulSum[, paste(grado, prueba, sep = "")]
  indProce  <- unique(gsub("(\\d\\w+)?(IT\\d+)", "\\1", 
                           oldMulSum[, paste(grado, prueba, sep = "")]))
  indProce  <- names(dirSalida)[!names(dirSalida) %in% indProce]
} else {
  indProce  <- names(dirSalida)
}

if (length(indProce) == 0) {
 cat("Ya se comprobo todo para establecimientos")
}

for (pp in indProce) {
  cat(">>>>>>> Calculando", pp, "<<<<<<<\n")
  dirLogCompro <- file.path(logPath, paste("Comprobacion-", pp,
                                           ".txt", sep = ""))
  sink(dirLogCompro)
  # # Lectura Consolidado
  filesConso <- nameConsoli[[pp]]
  if (length(filesConso) != 1) { 
    defLLave   <- c("NoHoja", "prueba")
    pegueConso <- pegarSalidas(filesConso, llave = NULL, "",
                               salBASES = FALSE, saveFile = FALSE)    
  } else {  
    pegueConso <- leer_LaF("./", filesConso) 
  }
  
  #  pegueConso <- pegueConso[!Jornada %in% sejosApgar[, codSitio], ]
  pegueConso[, score1   := as.numeric(score1)]
  pegueConso[, weight   := as.numeric(weight)]
  pegueConso[, disSenso := as.numeric(disSenso)]
  pegueConso  <- pegueConso[disSenso != "1" & copietas != "2", ] 
 
   
  # # Definiendo el grado y el codigo del ítem
  nomIndice  <- gsub("\\d(\\w+)", "\\1", pp)
  gradConso  <- gsub("(\\d)\\w+", "\\1", pp)

  cat(">>>>>>>>Revisando consolidado para ", nomIndice, "(", gradConso,
      ")\n", sep = "")
  fileEvalu  <- dirSalida[[pp]]
 
  # # Comparando con la base de codificación
  indGrado   <- codificaEscala[, paste("grado", gradConso, sep = ""), 
                              with = FALSE][[1]] == "1"
  codiFiltra <- codificaEscala[indGrado & NOMBRE %like% nomIndice, ]

  if (nrow(codiFiltra) != length(fileEvalu)) {
   stop("Falta un archivo de evaluación \n")  
  }

  codEscala  <- paste(gradConso, codiFiltra[, unique(cod_escala)], sep = "")
  filtroFT   <- ftEstab[[codEscala]]
  
  if (!nomIndice %in% nomEscaEsp) {
    indComplet <- unique(filtroFT[, tipo_reporte]) %in% 
                         c("01", "02", "08", "10", "11", "13", "15")
  } else {
    indComplet <- unique(filtroFT[, tipo_reporte]) %in% 
                         c("01", "02", "08", "10", "12", "14", "16") 
  }

  if (!all(indComplet)) {
    stop(">><< Los tipo de reporte en FT no corresponde con el tipo de",
         "reporte de la escala (Item o Indice)")
  }

  if (length(fileEvalu) != 1) { 
    defLLave   <- c("id_ent_territorial", "id_institucion", "id_sede", "tipo",
                    "subtipo", "cod_escala", "cod_item")
    pegueItems <- pegarSalidas(fileEvalu, defLLave, 
                               salBASES = FALSE, saveFile = FALSE) 
  } else {
    pegueItems <- leer_LaF("./", fileEvalu)
  }

  pegueItemsE <- pegueItems[id_institucion != "NULL" | id_sede != "NULL" , ]
  pegueItemsA <- pegueItems[id_institucion == "NULL" & id_sede == "NULL" , ]

  # # comprobación contenencia
  indFTCFE <- !filtroFT[, id_institucion] %in% pegueItemsE[, id_institucion]
  if (any(indFTCFE)) {
    noEstaFE <- filtroFT[indFTCFE & id_institucion != "NULL", ]
    noEstaFE <- noEstaFE[!tipo_reporte %in% c("01", "10"), ]  

    if (nrow(noEstaFE) != 0 ) {
      cat("Hay id_institucion en ficha tecnica con TR != 01 y != 10, que
          no estan en evaluación:\n")
      cat(paste(noEstaFE[, id_institucion], collapse = "\n"))
    }
  }
    
  indFECFT <- pegueItemsE[, id_institucion] %in% filtroFT[, id_institucion]
  if (any(!indFECFT)) {
    cat("Hay id_institucion en ficha evaluacion que no estan en FT: \n")  
    cat(paste(unique(pegueItemsE[!indFECFT, id_institucion]), 
              collapse = "\n"))
  }

  # Comprobación Ficha Tecnica Cognitivo 
  fichaTC  <- leer_LaF(nomDirec = basesPath, 
                       fileBase = "fichaTecnicaC2309_F.txt", 
                       carSepa = ",")
  fichaTC  <- fichaTC[anio == "2012" & grado %in% c("5", "9") & 
                      id_institucion != "NULL", ]

  indFTcCFTcc <- fichaTC[, id_institucion] %in% filtroFT[, id_institucion]
  if (any(!indFTcCFTcc)) {
    cat("Hay id_institucion en FT cognitivo que no estan en FT CC: \n")  
    cat(paste(unique(pegueItemsE[!indFECFT, id_institucion]), 
              collapse = "\n"))
  }


  # # comprobación identificadores    
  comCodIndice <- sapply(pegueItemsE[, list(cod_escala, cod_item)], unique)

  indEsc <- codiFiltra[, unique(cod_escala)] %in% comCodIndice[["cod_escala"]]
  indItm <- all(codiFiltra[, unique(cod_item)] %in% comCodIndice[["cod_item"]])
  if (!indEsc | !indItm) {
    stop("Hay problemas con el codigo de ítem y con el codigo de
         escala")
  } 

  # # Filtro FT y comprobación tipos 10 
  filTREST <- filtroFT[tipo_reporte %in% c("01", "10"), ] 
  firstCon <- conteoSI(filTREST, lookVal = c("0", "NULL"))

  if (nrow(filTREST) != 0) {
    comConteo(dataConteo = firstCon, newPrendTR = "", newPrendFA = "")

    indFTEVA <- filTREST[, id_institucion] %in% pegueItemsE[, id_institucion]
    if (any(indFTEVA)) {
      cat("+++++ Existen ", table(indFTEVA)["TRUE"],
          " establecimientos de ", length(indFTEVA), 
          " que estan en evaluación \n+++++ y no deberian estarlo.\n")
    }
  }

  # # Filtro FT y comprobación tipos 01, 02, 11 - 16

  filTREST <- filtroFT[tipo_reporte %in% c("01", "02", "11", "12", "13", 
                                           "14", "15", "16"), ] 
  firstCon <- conteoSI(filTREST, lookVal = "NULL")
  
  newPrendFA <- c("id_institucion", "total_participantes", "participantes",
                 "participantes_dis_cog", "tipo_reporte", "sedes")
  newPrendTR <- c("id_ent_territorial", "id_sede")

  comConteo(dataConteo = firstCon, newPrendTR = newPrendTR,
            newPrendFA = newPrendFA)

  # # Filtro FT y comprobación tipo 08
  filTREST <- filtroFT[tipo_reporte == "08", ] 
  firstCon <- conteoSI(filTREST, lookVal = c("0", "NULL"))

  newPrendFA <- c("id_sede", "total_participantes", "participantes",
                 "participantes_dis_cog", "tipo_reporte")
  newPrendTR <- c("id_institucion", "establecimientos")

  comConteo(dataConteo = firstCon, newPrendTR = newPrendTR,
            newPrendFA = newPrendFA)
  
  indFTEVA <- filTREST[, id_sede] %in% pegueItemsE[, id_sede]
  if (any(indFTEVA)) {
    cat("+++++ Existen ", table(indFTEVA)["TRUE"]," con codigo de
            sede", 
            length(indFTEVA), 
            " que estan en evaluación \n+++++ y no deberian estarlo.\n")
  } 

  # # Calculo Media Varianza
  sumPesos    <- pegueConso[, sum(weight), by = "prueba"]
  numChino    <- pegueConso[, length(NoHoja), by = "prueba"]
  scorePPeso  <- pegueConso[, sum(score1 * weight), by = "prueba"]
  scoreP2Peso <- pegueConso[, sum(score1 ^ 2 * weight), by = "prueba"]
  mediaChino  <- scorePPeso[, V1] / sumPesos[, V1]
  varChino    <- (scoreP2Peso[, V1] - sumPesos[, V1] * mediaChino^2) / 
                 (sumPesos[, V1] * ((numChino[, V1] - 1) / numChino[, V1]))    

  # # Comprobación tipo 11
  if (nomIndice %in% nomEscaNor) {
    # # Comparación multiplica y suma 
    multiplica <- sqrt(varNacional / varChino)
    suma       <- mediaNacional - multiplica * mediaChino

    fileMulSun <- musumSali[grep(paste(".+-", gradConso, "-", nomIndice,
                                       ".+", sep = ""), musumSali)]

    kMulSum        <- read.table(fileMulSun, header = FALSE)
    names(kMulSum) <- c("grado", "escala", "multiplica", "suma")
    multiplicaEV   <- kMulSum[, "multiplica"]
    sumaEV         <- kMulSum[, "suma"]    

    if (round(multiplica - multiplicaEV, 4) != 0 | 
        round(sumaEV - suma, 3) != 0) {
      stop("No cuadro el Multiplica y suma para la escala", nomIndice)
    }
    
    # #  Filtro Tipo de Reporte FT Estab
    filTREST <- filtroFT[tipo_reporte == "11", ] 
    filTREST <- filTREST[, list( id_institucion , grado, cod_escala, 
                                total_participantes, participantes ,
                                participantes_dis_cog)]

    # # comprobación por tipo - subtipos 
    pegueItemF  <- pegueItemsE[id_institucion %in% filTREST[, id_institucion], ]
    pegueFT2    <- pegueItemF[tipo == 2, ]
    secondCon   <- conteoSI(pegueFT2, lookVal = "NULL")
    comConteoRE <- comConteoEV(secondCon, newPrendTR = "cod_item", 
                               newPrendFA = "")

    pegueFT2    <- merge(filTREST, pegueFT2, all.x = TRUE,
                         by = c("id_institucion", "grado", 
                                "cod_escala"))

    if (any(!sapply(pegueFT2, function(x) 
                    is.na(table(is.na(x))["TRUE"])))) {
      stop("No estan todos los establecimientos en", namFile)
    }

    pegueFT2[, difN := as.numeric(N) - as.numeric(participantes)]
    indMal <- pegueFT2[, difN] != 0
    if (any(indMal)) {
      porcent <- round(sum(indMal)/length(indMal) * 100, 2)
      cat("---- El", porcent, "% de instituciones no cuadraron y son:")
      print(unique(pegueFT2[indMal, id_institucion]))
    } 
  }

  # # Comprobación tipo 12
  if (nomIndice %in% nomEscaEsp) {
    # # Definir Multiplica y Suma
    multiplica <- 100
    suma       <- 0 
    filTREST   <- filtroFT[tipo_reporte == "12", ] 

    # # Lectura cuantas consolidado
    namFile <- paste("countConsolidadoITEM.", nomIndice, ".", gradConso,
                     ".", "Rdata", sep = "")
    load(file.path("../../../CONSOLIDADO", namFile))
    conteoPart  <- rename(conteoPart, c("daneEstab" = "id_institucion"))
    pegeCodigos <- codiFiltra[, paste(cod_escala, cod_item, sep = "")]
    pegeCodigos <- paste(paste("'", codiFiltra[, NOMBRE], "' = '", 
                         pegeCodigos, "'", sep = ""), collapse = "; ")
    conteoPart[, prueba := recode(prueba, pegeCodigos)]
    conteoPart[, cod_escala := substr(prueba, 1, 3)]
    conteoPart[, cod_item := substr(prueba, 4, 6)]
    conteoPart[, prueba := NULL]
    conteoPart[, grado := gradConso]

    # # Comprobación por tipo - subtipos 
    pegueItemF  <- pegueItemsE[id_institucion %in% filTREST[, id_institucion], ]
    pegueFT2    <- pegueItemF[tipo == 2, ]
    nRowsAntes  <- nrow(pegueFT2)
    pegueFT2    <- merge(pegueFT2, conteoPart, all.x = TRUE,
                         by = c("id_institucion", "grado", 
                                "cod_escala", "cod_item"))
   
    if (any(!sapply(pegueFT2, function(x) 
                    is.na(table(is.na(x))["TRUE"])) | 
        nRowsAntes - nrow(pegueFT2)) != 0 ) {
      stop("No estan todos los establecimientos en", namFile)
    }

    pegueFT2[, difN := as.numeric(N) - part + participantes_dis_cog]
    indMal <- pegueFT2[, difN] != 0
    if (any(indMal)) {
      porcent <- round(sum(indMal)/length(indMal) * 100, 2)
      cat("---- El", porcent, "% de instituciones no cuadraron y son:")
      print(unique(pegueFT2[indMal, id_institucion]))
    }  

    # # Comprobando 
    pegueFT2    <- split(pegueFT2, f = pegueFT2[, cod_item])
    secondCon   <- lapply(pegueFT2, 
                          function(x) { conteoSI(x, lookVal = "NULL")})
    comConteoRE <- lapply(names(secondCon), 
                          function(x) {
                            cat("\n\n>>>>>>> Mirando", 
                                x, "<<<<<<<<<<\n\n")
                            comConteoEV(secondCon[[x]], 
                                        newPrendTR = "desviacion", 
                                        newPrendFA = "")})
    names(comConteoRE) <- names(secondCon)
  }

  # # Verificación de niveles Para Grandes
  if (nomIndice %in% nomEscaNor) {
    sumNiveles <- apply(pegueFT2[, list(nivel1, nivel2)], 1, 
                        function(x) sum(as.numeric(x)))
    indNiveles <- sumNiveles - 100 != 0
    if (any(indNiveles)) {
      warning(">>>>>>> Error en niveles para grandes (No suman 100) <<<<<<<", 
              immediate. = TRUE)  
    }
  }

  # # Funcion para verificar consolidados
  verInterv <- function(x) {
    difInter <- x[, minP][-1] - x[, maxP][-nrow(x)]
    if (nrow(x) >= 4) { 
      difInter <- difInter[- seq(3, nrow(x) - 1, by = 3)]
    }
    return(all(difInter >= 0))
  }

  if (nomIndice %in% nomEscaNor) {
    nomParams <- row.names(subset(secondCon, FALSE. != 0))
    nomParams <- nomParams[grep("^param.+", nomParams)]
    pegueFT2  <- list('NULL' = pegueFT2)
  } else {
    nomParams <- lapply(secondCon, function(x) 
                        row.names(subset(x, FALSE. != 0)))
    nomParams <- lapply(nomParams, function(x) x[grep("^param.+", x)])
    nomParams <- unique(unlist(nomParams))  
  }

  # # Comparación para los "param"
  for (uu in nomParams) {
    if (uu %like% "promedio") {
      varInter <- "puntuacion"
    } else {
      varInter <- "desviacion"
    }

    tipoToFil   <- data.frame(tableParamTipo[nom_param == uu, ])
    indColum    <- apply(tipoToFil[, c(-1, -2, -3)], 2, 
                           function(x) return(x != "null"))
    varAgrega   <- c(uu, "cod_item", names(indColum[indColum]))

    for (ww in names(pegueFT2)) {
      indEvalu    <- pegueFT2[[ww]][, uu, with = FALSE][[1]] != "NULL"
      pegueFT2Ent <- pegueFT2[[ww]][indEvalu,  c("id_institucion", varInter,
                                           "cod_item", uu), with = FALSE]
      pegueFT2Ent <- merge(pegueFT2Ent, infoEstab, by = "id_institucion", 
                           all.x = TRUE)   
      if (uu %like% "promedio") {
        comParams   <- pegueFT2Ent[, list(minP = min(as.numeric(puntuacion)), 
                                          maxP = max(as.numeric(puntuacion))), 
         by = varAgrega]
      } else {
        comParams   <- pegueFT2Ent[, list(minP = min(as.numeric(desviacion)), 
                                          maxP = max(as.numeric(desviacion))), 
         by = varAgrega]    
      }

      comParams   <- comParams[order(cod_item)]
      indSplit    <- apply(comParams[, varAgrega[-1], with = FALSE], 1,
                           function(x) paste(x, collapse = "-"))
      comParams   <- split(comParams[, c(uu, "minP", "maxP"), with = FALSE], 
                           f = indSplit)   
      comParamsQ   <- lapply(comParams, function(z) 
                             verInterv(z[order(z[, uu , with = FALSE][[1]])]))
      comParamsQ   <- unlist(comParamsQ)
      if (any(!comParamsQ)) {
        comParamsQ <- paste(c("",names(comParamsQ)[comParamsQ]), 
                            collapse = "\n       ")
        cat("+++++Hay traslape en el param1_promedio para:", comParamsQ)
      } 
    }
  }

  # # Comparación params NULL
  if (nomIndice %in% nomEscaNor) {
    comConteoRE <- list('NULL' = comConteoRE)
    # # Eliminar Pequeños 
    pegueConso <- pegueConso[!establecimiento %in% 
                             apagaSejos[[codEscala]][, id_institucion]]
  }

  for (oo in names(comConteoRE)) {
    comConteoREFil <- comConteoRE[[oo]]
    if (any(comConteoREFil[["1CP"]] %like% "^param2_.+$")) {
      compoNULL <- comConteoREFil[["1CP"]][-grep("^param2_.+$", 
                                                 comConteoREFil[["1CP"]])]
    } else {
      compoNULL <- comConteoREFil[["1CP"]]
    }

    pegueFT2FIL <- pegueFT2[[oo]][, c("id_institucion", compoNULL), 
                                  with = FALSE]
    conteoNULL  <- lapply(compoNULL, 
                         function(x) {
                           ind <-  pegueFT2FIL[, x, with = FALSE] == "NULL"
                           pegueFT2FIL[as.vector(ind), id_institucion]
                          })
    names(conteoNULL) <- compoNULL

    indComPais1 <- pegueFT2[[oo]][param2_desvest == "NULL", 
                                  desviacion] %in% c(0, "NULL")
    if (nomIndice %in% nomEscaEsp) {indComPais1 <- TRUE}
    indComPais2 <- pegueFT2[[oo]][param2_promedio == "NULL", 
                            puntuacion]  %in%
                   c(0, "NULL")
    if (any(!indComPais1) | any(!indComPais2)) {
      stop("O_O Problemas con la comparacion del pais O_O") 
    }
    
    for (xx in names(conteoNULL)) {
      if (uu %like% "promedio") {
        varInter <- "puntuacion"
      } else {
        varInter <- "desviacion"
      }

      cat("???? Revisando para --", xx, "-- ", gsub("NULL", "", oo), 
          "\n", sep = "")
      codEstabs    <- conteoNULL[[xx]]

      # # Definiendo que se entiende por paramn
      paramTipo    <- tableParamTipo[nom_param == xx,]
      tipoToFil    <- paramTipo[, kTipo]
      subtipoToFil <- paramTipo[, ksubTipo]

      paramTipo    <- rename(paramTipo, c("zonaEstab" = "zonaStab"))
      paramTipo    <- rename(paramTipo, c("ente_Id" = "enteTerr"))

      indFiltro    <- paramTipo[, list(zonaStab, sector, nivel)] != "null"
      indFiltro    <- names(indFiltro[1, ])[indFiltro]
      valToFind    <- apply(paramTipo[, indFiltro, with = FALSE], 1, 
                            function(x) paste(x, collapse = ""))

      filtroENT    <- unique(infoEstab[id_institucion %in% codEstabs,
                             ente_Id])
      filtroENTUNI <- pegueItemsA[tipo == tipoToFil & 
                                  subtipo == subtipoToFil & 
                                  cod_item == oo, ]
      filtroENTNO  <- filtroENT %in% filtroENTUNI[, id_ent_territorial]

      if (any(filtroENTNO)) {
        filtroENTUNI <- filtroENTUNI[id_ent_territorial %in% filtroENT, ]
        cat("      Existe registros en NULL pero hay ", 
            nrow(filtroENTUNI), " filas en Evaluación con información\n",
            "      del agregado de contraste.\n", sep = "")
        nEstabs <- filtroENTUNI[, as.numeric(N_Estab)] < 2
        
        # # Filtrando Consolidado
        nomItem   <- paste(nomIndice, gsub("i(\\d+)", "IT\\1", oo), sep = "")
        nomItem   <- gsub("NULL", "", nomItem)

        filtroCon <- pegueConso[prueba == nomItem,]    
        filtroVar <- apply(filtroCon[, indFiltro, with = FALSE],
                           1, function(x) paste(x, collapse = ""))        

        if (any(!nEstabs)) {
          enteProble <- filtroENTUNI[!nEstabs, id_ent_territorial]
          if (xx %like% "param1_") {
            indFiltro  <- "enteTerr"
            valToFind  <- enteProble
          }

          filtroFT2  <- merge(pegueFT2[[oo]][id_institucion %in% codEstabs],
                              infoEstab, by = "id_institucion")
          filtroFT2  <- filtroFT2[ente_Id %in% enteProble]
          indEstim0  <- filtroFT2[, varInter, with = FALSE][[1]] == "0" 

          if (any(!indEstim0)) {
            # # Comprobando que todos los chinos tengan el mismo puntaje
            filtroEst  <- filtroCon[enteTerr %in% enteProble &
                                    filtroVar == valToFind, 
                                    c("establecimiento", "enteTerr", 
                                      indFiltro, "score1"), with = FALSE]
            aFiltroEst <- filtroEst[, length(unique(score1)), 
                                          by = "enteTerr"]

            # # Comprobando que todos los colegios tengan el mismo
            # # promedio
            filtroCol <- pegueItemsE[id_institucion %in% 
                                     filtroEst[, establecimiento] & 
                                     cod_item == oo & tipo == 2, 
                                     list(id_institucion, puntuacion)]
            filtroCol <- merge(filtroCol, infoEstab, by = "id_institucion")
            filtroCol <- filtroCol[, length(unique(puntuacion)), 
                                             by = "ente_Id"]

            if (all(aFiltroEst[, V1]  == 1) | all(filtroCol[, V1]  == 1)) { 
              cat("     Hay NULL en la comparación con el agregado, sin\n",
                  "     embargo, tienen N_estab > 1 pero la desviación\n", 
                  "     del agregado es 0:", sep = "")
              cat(paste(c("", filtroENTUNI[!nEstabs, id_ent_territorial]), 
                        collapse = "\n              "))
            } else {
              cat(paste(c("", aFiltroEst[V1 != 1, enteTerr], 
                          filtroCol[V1 != 1, ente_Id],
                          collapse = "\n              ")))
              stop("--------------Si deberia haber comparación------------\n")
            }
          } else {
            cat("         -->Hay NULL en la comparación",
                "y N_estab > 1 se debe,\n",
                "            a que el valor a comparar es 0\n")
          }
          cat("\n")
        } 

        if (any(nEstabs)) {
          cat("         Los siguiente registro entidades-agregrado tienen\n",
              "         N_estab < 2 (UNI ESTABS), se deben eliminar? :") 
          codEnteUni <- filtroENTUNI[nEstabs, id_ent_territorial]
          if (xx %like% "param1_") {
            valToFind  <- codEnteUni
          }
          cat(paste(c("", codEnteUni), collapse = "\n              "))

          filtroVar <- apply(filtroCon[, indFiltro, with = FALSE], 
                             1, function(x) paste(x, collapse = ""))        
          filtroCon <- filtroCon[enteTerr %in% codEnteUni & 
                                 filtroVar == valToFind, 
                                 c("establecimiento", "enteTerr", indFiltro), 
                                 with = FALSE]
          estabPequ <- filtroCon[,length(enteTerr), by = establecimiento]
          estabPequ <- estabPequ[V1 < 6, establecimiento]

          filtroCon <- filtroCon[!establecimiento %in% estabPequ, 
                                 length(unique(establecimiento)), 
                                 by = c("enteTerr", indFiltro)]

          if (any(filtroCon[, V1] > 1)) { 
            stop("--------------Si deberia haber comparación--------------\n")
          }
          cat("\n")
        }
      } 

      if (any(!filtroENTNO)) {
        cat("      OK no hay registros y por ende no hay comparaciones, \n",
            "      para las siguientes entidades:", sep = "")
        cat(paste(c("", filtroENT[!filtroENTNO]), 
                  collapse = "\n              "))
        cat("\n")
        nomItem   <- paste(nomIndice, gsub("i(\\d+)", "IT\\1", oo), sep = "")
        nomItem   <- gsub("NULL", "", nomItem)
        filtroCon <- pegueConso[enteTerr %in% filtroENT[!filtroENTNO] & 
                                prueba == nomItem, indFiltro, with = FALSE]
        filtroCon <- apply(filtroCon, 1, function(x) paste(x, collapse = ""))
        cat("      Se busco ->", indFiltro, valToFind, "\n")
        if (valToFind %in% filtroCon) {
          stop("----------------Si deberia haber registro----------------\n")
        } 
      }
    }
  }
  
  # # Armando base "PtosCorteEscala.txt" 
  corteEscala <- nivelesDes[GRADO == gradConso & 
                             INDICE %like% nomIndice]
  corteEscala <- corteEscala[, list(INDICE, ALTO)]
  corteEscala[, ALTO := as.numeric(ALTO)]
  
  if (nomIndice %in% nomEscaNor) {
    corteEscala[, ITEM := 0]
  } else {
    corteEscala[, ITEM := 1]  
  }

  auxTable    <- corteEscala[, list(grado = gradConso, prueba = INDICE, 
                                    bajo = ALTO, media = mediaChino, 
                                    dest = sqrt(varChino),
                                    multiplica = multiplica, suma = suma,
                                    corteEscala = ALTO * multiplica +
                                    suma, Item = ITEM)]

  tableMulSum  <- auxTable

  # # Comprobación tipos 13 y 14
  filTREST    <- filtroFT[tipo_reporte %in% c("13", "14"), ] 
  pegueItemF  <- pegueItemsE[id_institucion %in% filTREST[, id_institucion], ]
  pegueFTPeq  <- pegueItemF[tipo == 21, ]
  tercCon     <- conteoSI(pegueFTPeq, lookVal = "NULL")

  newPrend    <- c("param1_promedio", "puntuacion", "margen", "linf", 
                   "lsup", "indicadora_EE", "desviacion", 
                   "param2_promedio", "param4_promedio", "param7_promedio", 
                   "param8_promedio", "param9_promedio", "param10_promedio", 
                   "param11_promedio", "param12_promedio", "param1_desvest", 
                   "param2_desvest", "param4_desvest", "param7_desvest", 
                   "param8_desvest", "param9_desvest",
                   "param10_desvest", "param11_desvest", "param12_desvest")

  if (nomIndice %in% nomEscaNor) {
    newPrend  <- c(newPrend, "cod_item")
  }
  comConteoRE <- comConteoEV(tercCon, newPrendTR = newPrend, newPrendFA = "")

  if (sum(sapply(comConteoRE, length)) != 0) {
    stop(">>>>>>>>>  Error en Pequeños <<<<<<<<<<")
  }

  if (nomIndice %in% nomEscaNor) {
    pegueFTPeq <- merge(pegueFTPeq, filTREST, all.x = TRUE,
                         by = c("id_institucion", "grado", "cod_escala")) 
    pegueFTPeq <- pegueFTPeq[, difN := as.numeric(N) - 
                                        as.numeric(participantes)]
  } else {
    pegueFTPeq <- merge(pegueFTPeq, conteoPart, all.x = TRUE,
                        by = c("id_institucion", "grado", "cod_escala", 
                               "cod_item"))
    pegueFTPeq <- pegueFTPeq[, difN := as.numeric(N) - part + 
                                       participantes_dis_cog]
  }

  indNoCru <- !sapply(pegueFTPeq, function(x) is.na(table(is.na(x))["TRUE"]))
  if (any(indNoCru)) {
    stop("No estan todos los establecimientos en", namFile)
  }

  indMal <- pegueFTPeq[, difN] != 0
  if (any(indMal)) {
    porcent <- round(sum(indMal)/length(indMal) * 100, 2)
    cat("---- El", porcent, "% de instituciones no cuadraron y son:")
    print(unique(pegueFTPeq[indMal, id_institucion]))
  }
   
  # # Comprobación tipos 15 y 16
  filTREST    <- filtroFT[tipo_reporte %in% c("15", "16"), ] 
  pegueItemF  <- pegueItemsE[id_institucion %in% filTREST[, id_institucion], ]
  pegueFTDis  <- pegueItemF[tipo == 3, ]
  cuartoCon   <- conteoSI(pegueFTDis, lookVal = "NULL")
  comConteoRE <- comConteoEV(cuartoCon, newPrendTR = newPrend, newPrendFA = "")

  if (sum(sapply(comConteoRE, length)) != 0) {
    stop(">>>>>>>>>  Error en Discapacitados <<<<<<<<<<")
  }

  if (nomIndice %in% nomEscaNor) {
    pegueFTDis <- merge(pegueFTDis, filTREST, all.x = TRUE,
                        by = c("id_institucion", "grado", 
                               "cod_escala")) 
    pegueFTDis <- pegueFTDis[, difN := as.numeric(N) - 
                             as.numeric(participantes_dis_cog)]
  } else {
    pegueFTDis <- merge(pegueFTDis, conteoPart, all.x = TRUE,
                        by = c("id_institucion", "grado", "cod_escala", 
                               "cod_item"))
    pegueFTDis <- pegueFTDis[, difN := as.numeric(N) - 
                             participantes_dis_cog]
  } 

  indMal <- pegueFTDis[, difN] != 0
  if (any(indMal)) {
    porcent <- round(sum(indMal)/length(indMal) * 100, 2)
    cat("---- El", porcent, "% de instituciones no cuadraron y son:")
    print(unique(pegueFTDis[indMal, id_institucion]))
  } 

  # Comparación de niveles pequeños o Discog para indices que se reporta
  # normal
  pegueFTPD  <- rbind(pegueFTPeq, pegueFTDis)
  sumNiveles <- apply(pegueFTPD[, list(nivel1, nivel2)], 1, 
                      function(x) sum(as.numeric(x)))

  if (nomIndice %in% nomEscaNor) {
    indNiveles <- sumNiveles - pegueFTPD[, as.numeric(N)] != 0
  } else {
    indNiveles <- sumNiveles > pegueFTPD[, as.numeric(N)] 
  }
  if (any(indNiveles)) {
    stop(">>>>>>>>>> Error en niveles (pequeños o Discog) <<<<<<<<<<")  
  }
  unlink(dirLogCompro)
  sink()

  # # Guardar Multiplica y Suma
  guardarListaFT(tableMulSum, dirFilMulSum, basesPath = "./")
  rm(list = ls()[!ls() %in% c(varIniciales, "pp", "varIniciales")]) 
  gc(reset = TRUE)
}

