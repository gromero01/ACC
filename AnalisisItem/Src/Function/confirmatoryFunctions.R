################################################################################
# # univariateFunctions.R
# # R Versions: 3.2.3 x64
# #
# # Author(s): Jorge Mario Carrasco
# #
# # SB ALL
# # Description: Function to compute SEM analysis
# #              and make some result in Excel
# #
# # Inputs: NA
# #
# # Outputs: NA
# #
# # File history:
# #   20160601: Creation, adpatación codigos de 04Exploratorio
# #   20140201: 
################################################################################

##############################################################
# # Function to make SEM analysis
##############################################################
generateConfirmatory <- function(flagUni, flagCor, flagBiFac, nameTest = "Constructo") {
   # # Make exploratory analisis
   # #
   # # Arg:
   # #  flagUni[boolean]: if you want run Unidimensional Model
   # #  flagCor[boolean]: if you want run Multidimensional Correlated Model
   # #  flagBiFac[boolean]: if you want add second latent cap
   # # Ret:
   # #  the variable after recode
   # #  levels of variable    

   if (flagUni & flagCor) {
     stop("_______ERROR________ Debe solo existir una bandera prendida", 
          "'flagUni' (Unidimensional) o 'flagCor' (Multidimensional", 
          "Correlacionado)\n")
   }
   flagCor <- ifelse(flagBiFac, TRUE, FALSE)

   blockStruc         <- dictVarPrueba[, c("idx", "indice")]           
   if (flagUni) {
     blockStruc[, 'indice'] <- nameTest
   }               
   names(blockStruc)  <- c("indicator", "factor")
   structureBlock <- GenerateStructure(blockStruc)
   
   # # Check if any index is defined under a Rasch model and fix its factor loadings to one
   dicRasch        <- subset(dictVarPrueba, codMod %in% codesRasch)[, c("idx", "indice")]
   names(dicRasch) <- c("indicator", "factor")
   if (nrow(dicRasch) > 0) {
     fixedStructBlock <- data.frame(dicRasch, loading = 1)
     fixedModBlock  <- GenerateConstrainsStructure(fixedStructBlock)
   } else {
     fixedModBlock  <- NULL
   }      
   
   # # Generate factor model
   fixStructBlock <- factorStructure2LavaanModel(structureBlock,
                                                 fixedLoadings = fixedModBlock)
   if (flagBiFac){
     fixStructBlock[lenghtIndices+1] <- paste(nameTest , "=~",
                                              paste(names(fixStructBlock),
                                              collapse = " + "))
   }
   # # Run dimensional model
   modBlock <- try(cfa(fixStructBlock,
                   sample.cov = corConfBlockX,
                   sample.nobs = nObsConfirmatory,
                   std.lv = TRUE, estimator = "ML",
                   orthogonal = flagCor & !flagUni))

   labelAux <- ifelse(flagUni, "uniG", ifelse(flagCor & !flagBiFac, "MultNCG"), 
                      ifelse(flagBiFac, "bifaG", "MultG"))
   sumModMult <- parameterEstimates(modBlock)
   outGraph   <- file.path(outPathPbaGraph, paste0(labelAux, "_", kk, "_",  
                             versionOutput, sep = ''))
   if (flagBiFac) {
     grapSE <- try(semPaths(modBifactorialBlock, "std", edge.label.cex=0.5,
                   curvePivot = FALSE, style = "lisrel", rotation = 4,
                   layout = "tree2", edge.color = "black",
                   optimizeLatRes = FALSE, sizeLat = 5,
                   filetype = kExt, intStyle = "single",
                   arrowAngle = pi/16, sizeMan = 5,
                   filename = outGraph,
                   nCharNodes = max(nchar(names(confkkBlockX)))))
   } else {
     grapSE <- semPaths(modBlock, "std", style = "OpenMx", sizeMan = 7,
                        edge.label.cex=0.8, curvePivot = TRUE, layout = "circle",
                        edge.color = "black", filename = outGraphMult, filetype =
                        "png", nCharNodes = ncol(corConfBlockX))))
   }
   outFitMeas <- try(MakeSummary(modBlock))
   return(list('sumModMult' = sumModMult, 'outGraph' = outGraph, 'outFitMeas' = outFitMeas))
}  

##############################################################
# # Function to create a sheet in exploratory output analysis
##############################################################

CreateExcel <-  function(nameSheet, summary, outGraph, 
                         outFits, items, model,
                         outfit){             
  # # nameSheet
  assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))
  
  # # salidas del modelo 
  if(model) {
    # # Adición de la tabla de resumen del modelo
    nRow <- nrow(summary)
    summaryUni <- data.frame(summary)
    addDataFrame(summary, sheet = get(nameSheet), startRow = 7,
                 startColumn = 1, row.names = FALSE,
                 col.names = TRUE, colnamesStyle = csEnc,
                 colStyle = list('2' = cen, '4' = csN, '5' = csN, '6' = csN, '7' = csN,
                                 '8' = csN, '9' = csN)
                 )
    # # Definición del ancho de las columnas
    xlsx::setColumnWidth(get(nameSheet), 1, 20)
    xlsx::setColumnWidth(get(nameSheet), 2, 15)
    xlsx::setColumnWidth(get(nameSheet), 3, 20)
    xlsx::setColumnWidth(get(nameSheet), 4, 20)
    xlsx::setColumnWidth(get(nameSheet), 5, 12)
    xlsx::setColumnWidth(get(nameSheet), 6, 12)
    xlsx::setColumnWidth(get(nameSheet), 7, 12)
    xlsx::setColumnWidth(get(nameSheet), 8, 12)
    xlsx::setColumnWidth(get(nameSheet), 9, 12)
    
    # # Adición de la gráfica
    nRow <- nrow(summaryUni)
    
    addPicture(file = paste(outGraph, kExt, sep = '.'),
               sheet = get(nameSheet), scale = 1.3,
               startRow = nRow + 9, startColumn = 1)
  }
  
  # # Salida del resúmen de los items
  if(items) {
    varKeepDict <- c('id', 'indice', 'etiqu')
    if (object@exam == "SABER359") {
      dictVarPruebaInd[, "etiqu"] = "No disponible"
    }
    confItemIndex <- dictVarPruebaInd[, varKeepDict]
    isVacio <- confItemIndex[, 'etiqu'] == '' | is.na(confItemIndex[, 'etiqu'])
    if (any(isVacio)) {
      confItemIndex[isVacio, 'etiqu'] <- confItemIndex[isVacio, 'instr']
    }
    
    addDataFrame(confItemIndex, sheet = get(nameSheet), startRow = 7,
                 startColumn = 1, row.names = FALSE,
                 col.names = TRUE, colnamesStyle = csEnc)
    
    xlsx::setColumnWidth(get(nameSheet), 1, 20)
    xlsx::setColumnWidth(get(nameSheet), 2, 15)
    xlsx::setColumnWidth(get(nameSheet), 3, 20)
    xlsx::setColumnWidth(get(nameSheet), 4, 20)
  }
  
  # # Salida de los ajustes
  
  if(outfit) {
    tableAdjust  <- data.frame(outFits)
    if(ncol(tableAdjust) != 1) {
      
      if (controlAnal@param$flagBiFac) {
        colnames(tableAdjust) <- c('Unidimensional', 'Multidimensional',
                                   'MultidimensionalNC', 'Bifactorial')
      } else {
        colnames(tableAdjust) <- c('Unidimensional', 'Multidimensional',
                                   'MultidimensionalNC')
      }
      
      # # Adición de la tabla de resumen del modelo
      nRow <- nrow(tableAdjust)
      addDataFrame(tableAdjust, sheet = get(nameSheet), startRow = 7,
                   startColumn = 1, row.names = TRUE,
                   col.names = TRUE, colnamesStyle = csEnc,
                   colStyle = list('1' = csN, '2' = csN, '3' = csN,
                                   '4' = csN, '5' = csN))
      
      xlsx::setColumnWidth(get(nameSheet), 1, 20)
      xlsx::setColumnWidth(get(nameSheet), 2, 20)
      xlsx::setColumnWidth(get(nameSheet), 3, 20)
      xlsx::setColumnWidth(get(nameSheet), 4, 20)
      xlsx::setColumnWidth(get(nameSheet), 5, 20)
    }
    
    if(ncol(tableAdjust) == 1) {
      colnames(tableAdjust) <- 'Unidimensional'
      # # Adición de la tabla de resumen del modelo
      nRow <- nrow(tableAdjust)
      addDataFrame(tableAdjust, sheet = get(nameSheet), startRow = 7,
                   startColumn = 1, row.names = TRUE,
                   col.names = TRUE, colnamesStyle = csEnc,
                   colStyle = list('1' = csN))
      
      xlsx::setColumnWidth(get(nameSheet), 1, 20)
      xlsx::setColumnWidth(get(nameSheet), 2, 15)
      xlsx::setColumnWidth(get(nameSheet), 2, 20)
      xlsx::setColumnWidth(get(nameSheet), 2, 20)
    }
  }
  
  namesPrueba <- subset(pruebasDesc, codigo_prueba == kk,
                        select = c(codigo_prueba, prueba))
  namesPrueba[, 'nItems'] <-  nrow(dictVarPruebaInd)
  
  criterios1   <- c('Código de Prueba', 'Datos de Análisis',
                    'Uso criterio Omisiones', 'Número de ítems',
                    'Tipo de Análisis')
  
  valores1     <-  data.frame(valor = c(namesPrueba[, 'codigo_prueba'],
                                        ifelse(isCensal, 'Censal', 'Muestra Nacional'),
                                        ifelse(isOmissDel, 'Sí', 'No'),
                                        namesPrueba[, 'nItems'], useMatrixCorr ))
  cabBlock1    <- data.frame(criterios1, valores1)
  addDataFrame(cabBlock1, sheet = get(nameSheet), startRow = 1,
               startColumn = 1, row.names = FALSE,
               col.names = FALSE, colStyle = list('1' = csNeg))
  
  criterios2 <- c('Prueba','n Análisis',
                  'Criterio para tratamiento de omisiones',
                  'Tratamiento de Valores Ausentes', 'Comentario')
  
  valores2   <-  data.frame(valor = c(namesPrueba[, 'prueba'],
                    nObsConfirmatory, kOmissionThreshold,
                    versionComment, useCor))
  
  cabBlock2  <- data.frame(criterios2, valores2)
  
  addDataFrame(cabBlock2, sheet = get(nameSheet), startRow = 1,
               startColumn = 3, row.names = FALSE,
               col.names = FALSE, colStyle = list('1' = csNeg))
}

##############################################################
# # Function to make Summary GFIS SEM Model
##############################################################


MakeSummary <- function(fitModel) {
  meas1    <- fitMeasures(fitModel)
  meas2    <- moreFitIndices(fitModel)
  mCdonald <- exp(-1/2 * (meas1["df"] / meas1["ntotal"]))
  chi_df   <- meas1["chisq"] / meas1["df"]
  outFitM  <- rbind(data.frame(meas=meas1),
                    data.frame(meas=meas2),
                    mCdonald = mCdonald,
                    chi_df = chi_df)
  outFitM  <- data.frame(measure = rownames(outFitM), meas = outFitM)
  
  index1 <- c("npar", "ntotal",
              "chisq", "df", "pvalue", "chi_df",
              "aic", "bic", "bic2", "bic.priorN",
              "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
              "srmr", "srmr_nomean",
              "cfi", "nfi", "ifi",
              "gammaHat", "adjGammaHat",
              "mCdonald")
  
  index2 <- c("npar", "ntotal",
              "chisq", "    df", "    pvalue", "    chi_df",
              "aic", "bic", "bic2", "bic.priorn",
              "rmsea", "    rmsea.ci.lower", "    rmsea.ci.upper", "    rmsea.ci.pvalue",
              "srmr", "    srmr_nomean",
              "cfi", "nfi", "ifi",
              "gammahat", "    adjgammahat",
              "mcdonald")
  
  isMeasure <- outFitM[, 'measure'] %in% index1
  
  outFitM   <- data.frame(outFitM[isMeasure, ] )
  
  outFitM   <- data.frame(outFitM[index1, 2])
  rownames(outFitM) <- toupper(index2)
  colnames(outFitM) <- 'Measures'
  outFitM
}