################################################################################
# # dimensionalityCC.Rnw
# # R Versions: R version 3.02
# #
# # Author(s): María Fernanda Zárate Jiménez
# #
# #
# # Process: SABER 5° y 9° Factores Asociados
# # Description: Genera salidas en excel para llevar a cabo análisis
# #              confirmatorios de dimensionalidad
# # Inputs: datBlock, dictionaryList, confirmatorySample, corConfirmatory.
# #
# # Outputs: confirmatorySample.R, exploratorySample.R, corExploratory.R,
# #          corConfirmatory.R, 05Confirmatorio.xlsx
# #
# # File history:
# #   20111129: Creation
# #   20121126: Lecutura Prueba actividades 5. Paola Marin
# #   20121217: Tablas con diferentes medidas de ajuste para comparacion
# #   de modelos.Paola Marin
# #   20130107: Adaptación para correr FA
# #   20140505: Adaptación para generar salidas en Excel
# #   20150505: Inclución de procesos para poder correer SABER 3, 5 y 9 junto con
# #             SABER 11 y SABER PRO. Jorge Mario Carrasco.
###############################################################################
# setwd("\\\\icfesserv5/academica$/SABER/SABER_2013/20132-EJERCICIO_ANALITEM359-JUNIO2")
options(encoding = "UTF-8")

################################################################################
# # global paths
################################################################################
inPath        <- file.path("Input")
funPath       <- file.path("Src", "Function")
outPath       <- file.path("Output", "05Confirmatorio")
logPath       <- file.path("Log")
outPathSamp   <- file.path("Output", "Muestras")
inPathSam     <- file.path("Input", "Muestras")

################################################################################
# # Command line parameters
################################################################################
cat("-------------- Lectura de Archivos -----------------\n")
# # Lectura de parámetros
args <- commandArgs();

# #  check if --args used. This avoids a problem with earlier versions of R
argsPos  <- match("--args", args)
codeName <- gsub("--file=Src(\\\\)?", "", args[grep("--file", args)])

# #  Parameters extraction
if(!is.na(argsPos) && length(args) > argsPos){ 
  controlFile <- args[argsPos + 1];  # Class with parameters
  # controlFile <- "controlData.Rdata"; codeName = "05Confirmatorio.R"
} else {
  cat("Parametros de la función:\n")
  cat("----> controlData: [Rdata] Class with parameters\n")
  stop("**ERROR**  en los parametros")
}

# # Cargando parametros de las pruebas
source(file.path(funPath, "pruebaClass.R"))
load(file.path(inPath, controlFile))

################################################################################
# # global definitions
################################################################################

setGeneric(name = "confirmatAnalysis", def = function(object){standardGeneric("confirmatAnalysis")})

# object <- controlData[[prueba]]
# object <- controlData[['./SABER9/SA20131/JP_M10_M11_M15_M16_M17_M19_M20_M21_M22_M23_M24_M3/PBAM10']]
setMethod("confirmatAnalysis", "Prueba",
          function(object){
            # # isCensal: Tipo de Análisis
            # # isTypeB: Tipo de Estudio  
            # # nReplicates: Number of iterations for parallel analysis
            # # useCor: use parameter for correlations
            # # useMatrixCorr: use type of matrix
            # # kApli: tipo de aplicacion 1 = Censal, 2 = Control, 3 = SobreMuestra,
            # #        4 = Especial, 5 = Adicional Censal, 6 = Adicional Control
            
            isCensal = FALSE; isTypeB  = FALSE; useCor = "pairwise.complete.obs"; 
            useMatrixCorr = 'policoricas' 
            kApli = c(2, 3, 4, 6)
            
            # # deleted students with more than 80% of omission for the topic
            # # in the items that are not eliminated
            kOmissionThreshold  = 0.8
            
            # # Categories will consider as No Response
            catToNA    = c('No Presentado', 'NR', 'Multimarca')
            
            # # Categories wich are Rasch Models
            codesRasch = c('01', '02')
            
            # # cod for 'no eliminated' items
            kCodNElim = '06'
            
            # # cod for 'paralel' items
            kCodPar   = '01'
            
            # # cod for 'noModel'
            kCodMod   = '00'
            
            # # cod for Graphs
            kExt = 'png'
            
            ###############################################################################
            # # Load libraries
            ################################################################################
            require(data.table)   # # 1.5-6
            require(xtable)   # # 1.5-6
            require(polycor)  # # 0.7-8
            require(mc2d)     # # 0.1-12
            require(ggplot2)  # # 0.8.9
            require(lavaan)   # # 0.4-10
            require(semTools) # # 0.2-8
            require(car)
            require(semPlot)  # # 0.2-8
            require(GPArotation)
            require(xlsx)
            require(plyr)
            require(reshape)
            
            ################################################################################
            # # Load sourcefiles
            ################################################################################
            source(file.path(funPath, "MeasurementModelsWithLavaan.R"))
            source(file.path(funPath, "corItem.R"))
            source(file.path(funPath, "univariateFunctions01.R"))
            
            ###########################################################################
            # # Other Functions
            ###########################################################################
            
            CreateExcel <-  function(nameSheet, summary, outGraph, outFits, items, model,
                                     outfit){
              
              # # nameSheet
              assign(nameSheet, createSheet(wb, sheetName = nameSheet))
              
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
                setColumnWidth(get(nameSheet), 1, 20)
                setColumnWidth(get(nameSheet), 2, 15)
                setColumnWidth(get(nameSheet), 3, 20)
                setColumnWidth(get(nameSheet), 4, 20)
                setColumnWidth(get(nameSheet), 5, 12)
                setColumnWidth(get(nameSheet), 6, 12)
                setColumnWidth(get(nameSheet), 7, 12)
                setColumnWidth(get(nameSheet), 8, 12)
                setColumnWidth(get(nameSheet), 9, 12)
                
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
                
                setColumnWidth(get(nameSheet), 1, 20)
                setColumnWidth(get(nameSheet), 2, 15)
                setColumnWidth(get(nameSheet), 3, 20)
                setColumnWidth(get(nameSheet), 4, 20)
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
                  
                  setColumnWidth(get(nameSheet), 1, 20)
                  setColumnWidth(get(nameSheet), 2, 20)
                  setColumnWidth(get(nameSheet), 3, 20)
                  setColumnWidth(get(nameSheet), 4, 20)
                  setColumnWidth(get(nameSheet), 5, 20)
                }
                
                if(ncol(tableAdjust) == 1) {
                  colnames(tableAdjust) <- 'Unidimensional'
                  # # Adición de la tabla de resumen del modelo
                  nRow <- nrow(tableAdjust)
                  addDataFrame(tableAdjust, sheet = get(nameSheet), startRow = 7,
                               startColumn = 1, row.names = TRUE,
                               col.names = TRUE, colnamesStyle = csEnc,
                               colStyle = list('1' = csN))
                  
                  setColumnWidth(get(nameSheet), 1, 20)
                  setColumnWidth(get(nameSheet), 2, 15)
                  setColumnWidth(get(nameSheet), 2, 20)
                  setColumnWidth(get(nameSheet), 2, 20)
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
            # #
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
            
            controlPrueba <- object
            controlAnal   <- controlPrueba@Analisis[[codeName]]
            
            # # version of input response strings information and sample
            versionOutput <- object@verSalida
            verDataIn     <- object@verEntrada
            
            versionComment <- paste0("Corrida Análisis Confirmatorio --",  object@nomPrueba, "--")
            ################################################################################
            # # load data
            ################################################################################
            # # output from 00CrearRdata.R
            load(object@pathDic)
            load(object@pathRdata)
            gradoPba <- gsub(".+List_GR\\.(\\d).+", "\\1", object@pathDic)
            nomPba   <- gsub(".+List_GR\\.(\\d)(pba|PBA)(\\w).+", "\\3", object@pathDic)
            ################################################################################
            # # Adjusting the DB
            ###############################################################################
            
            # # conserved data from sample application
            
            if(object@exam == "ACC") {
              if(isTypeB) {
                if (!is.data.frame(datBlock) & is.list(datBlock) & length(datBlock) == 1) {
                  datBlockControl <- list(subset(datBlock[[1]], x$tipoApli %in% kApli))
                  names(datBlockControl) <- names(datBlock)
                } else {
                  datBlockControl <- lapply(datBlock, function(x)
                    subset(x, x$tipoApli %in% kApli))
                }
              } else {
                datBlockControl <- datBlock
              }
            } else {
              datBlockControl <- lapply(datBlock, function(x) {
                aux <- data.frame(x$calBlock)
                names(aux) <- gsub("X(\\d+)", "\\1", names(aux))
                return(aux)})
            }
            
            # # obtener los códigos de las pruebas leídas que corresponden a las
            # # pruebas que se desean analizar
            
            filtroDicc <- function(x, varsKeep) {
              isNoElim    <- x[, 'elimina' ] == kCodNElim
              isCodPar    <- x[, 'paralelo'] == kCodPar
              isCodMod    <- x[, 'codMod'  ] != kCodMod
              
              if (object@exam == "ACC") {
                isNI        <- x[, 'indice'  ] != 'NI' 
                isPrueba    <- isNoElim & isCodPar & isCodMod & isNI
              } else {
                isPrueba    <- isNoElim & isCodPar & isCodMod
              }
              return(x[isPrueba, varsKeep])
            }
            
            pruebasDesc <- unique(filtroDicc(dictionaryList$variables, c('codigo_prueba', 'prueba')))
            exist       <- pruebasDesc[, 'codigo_prueba'] %in% gsub("\\.con", "", names(datBlock))
            pruebasRead <- pruebasDesc[exist, 'codigo_prueba']
            pruebasRead <- sort(pruebasRead)
            
            if(!file.exists(outPathSamp)) {
              dir.create(outPathSamp)
              cat("Se creo la carpeta muestras en el output!!!!!\n")
            }
            
            ################################################################################
            # # Analisis de Dimensionalidad
            ################################################################################
          
            # # create list to save results
            datBlockOmis <- list()
            pruebasRead <- pruebasRead[!grepl("(pba|PBA)", pruebasRead)]
            resulConf   <- list()
            for (kk in pruebasRead) {
              
              # #  carpetas por prueba
              outPathPba <- file.path(outPath, paste0("pba", gradoPba, nomPba), paste(kk, sep = '_')) 
              # #  carpetas de muestras por prueba
              outPathSamPba <- file.path(outPathSamp, paste0("pba", gradoPba, nomPba), paste(kk, sep = '_'))
              # #  carpetas de muestras completa
              outPathtotal <- file.path(outPathSamPba, "total")
              
              nomKK <- paste0(gradoPba, nomPba, "_", kk)
              if(!file.exists(outPathSamPba)) {
                dir.create(outPathSamPba, recursive = TRUE)
                dir.create(outPathtotal, recursive = TRUE)
                cat("Se creo la carpeta ", nomKK, 
                    "total y toInvariance en muestras\n")
              }
              
              if (!file.exists(outPathPba)) {
                dir.create(outPathPba, recursive = TRUE)
              }
              
              # # keep items that aren't eliminated
              dictVarPrueba    <- subset(dictionaryList$variables, codigo_prueba == kk)
              dictVarPruebaInd <<- filtroDicc(dictVarPrueba, names(dictVarPrueba))
              varId            <- dictVarPruebaInd[, 'id']
              
              # # variables by index
              dictKk <- subset(dictVarPruebaInd, TRUE, select = c(id, indice))
              if (any(!(dictKk[, 'id'] %in% names(datBlockControl[[paste0(kk, ".con")]])))) {
                stop("No estan todas las variables de datBlock")
              }
              
              if (nrow(dictKk) == 0) {
                stop('No tiene Items para hacer la corrida en PBA', nomKK,
                      'Revise si todos los Items estan eliminados o si tiene alguna
                      escala diferente a NI')
              }
              
              # # Recode 'Multimarca' 'No aplica' 'NR' for NA
              nameNode <- names(datBlockControl)[names(datBlockControl) %like% kk]
              datBlockOmis[[kk]]  <- datBlockControl[[paste0(kk, ".con")]]
              if (object@exam == "ACC") {
                datBlockOmis[[kk]][, varId] <- lapply(datBlockOmis[[kk]][, varId],
                                                      RecodeToNA, catToNA)
                
                datBlockOmis[[kk]] <- rename(datBlockOmis[[kk]], c(noHoja="consLect"))
              } else {
                  datBlockOmis[[kk]][, varId]  <- lapply(datBlockOmis[[kk]][, varId], function(x) ordered(x))
                  if (object@exam %in% c("SABERPRO", "SABER359")) {
                    datBlockOmis[[kk]] <- rename(datBlockOmis[[kk]], c(SNP="consLect"))
                  }
              }
              
              kkBlock        <- datBlockOmis[[kk]]
              kkBlock        <- kkBlock[, c('consLect', varId)]
              
              # # conserve rows with less than kOmissionThreshold NR data
              isOmissDel <- kOmissionThreshold <= 1 & kOmissionThreshold > 0
              if (isOmissDel) {
                subCon     <- kkBlock[, varId]
                if (object@exam == "ACC") {
                  subCon[, ] <- lapply(subCon, RecodeToNA, catToNA)
                }
                
                misRow     <- rowMeans(is.na(subCon))
                isKeep     <- misRow <= kOmissionThreshold
                kkBlock    <- kkBlock[isKeep, ]
              }
              
              # # Number of observations
              nObsBlock <- nrow(kkBlock)
              
              # # Obtain sample
              confData <- file.path(outPathSamPba, "total",
                                    paste("confirmatorySample_", nomKK, "_V",
                                          verDataIn, ".RData", sep = ""))
              expData  <- file.path(outPathSamPba, "total",
                                    paste("exploratorytorySample_", nomKK, "_V",
                                          verDataIn, ".RData", sep = ""))
              
              if (!file.exists(confData) & !file.exists(expData)) {
                isExploratory    <- sample(x = rownames(kkBlock), size = nObsBlock %/% 2)
                expkkBlock       <- subset(kkBlock[isExploratory, ], select = -consLect)
                rownames(expkkBlock) <- datBlockOmis[[kk]][isExploratory, ]$consLect
                nObsExploratory  <- nrow(expkkBlock)
                
                isConfirmatory        <- -as.numeric(isExploratory)
                confkkBlock           <- subset(kkBlock[isConfirmatory, ], select = -consLect)
                rownames(confkkBlock) <- kkBlock[isConfirmatory, ]$consLect
                nObsConfirmatory      <- nrow(confkkBlock)
                attr(expkkBlock, 'fechaDescarga')  <- attr(expkkBlock, 'fechaDescarga')
                attr(expkkBlock, 'prueba')         <- attr(datBlock[[kk]], 'prueba')
                attr(confkkBlock, 'fechaDescarga') <- attr(expkkBlock, 'fechaDescarga')
                attr(confkkBlock, 'prueba')        <- attr(datBlock[[kk]], 'prueba')
                save(expkkBlock, nObsExploratory, file = expData)
                save(confkkBlock, nObsConfirmatory, file = confData)
              } else {
                load(confData)
              }
              
              # # Obtain correlation matrix
              
              corExpData   <- file.path(outPathSamPba, "total",
                                        paste("corExploratory_", nomKK, "_V",
                                              verDataIn, ".RData", sep = ""))
              
              corConfData  <- file.path(outPathSamPba, "total",
                                        paste("corConfirmatory_", nomKK, "_V",
                                              verDataIn, ".RData", sep = ""))
              
              if (!file.exists(corExpData) & !file.exists(corConfData)) {
                corExpBlock  <- hetcor(expkkBlock, pd = TRUE, use = useCor, std.err =
                                         FALSE, ML = FALSE)
                corConfBlock <- hetcor(confkkBlock, pd = TRUE, use = useCor, std.err =
                                         FALSE, ML = FALSE)
                if(class(corConfBlock) != "try-error" & class(corExpBlock) != "try-error") {
                  attr(corExpBlock, 'fechaDescarga')  <- attr(datBlock[[kk]], 'fechaDescarga')
                  attr(corConfBlock, 'prueba')        <- attr(datBlock[[kk]], 'prueba')
                  save(corExpBlock,  file = corExpData)
                  save(corConfBlock, file = corConfData)
                } else {
                  cat("No se estimo la matriz policórica inicial\n\n")
                  controlAnal@param$flagUni     <- FALSE
                  controlAnal@param$flagMultiC  <- FALSE
                  controlAnal@param$flagMultiNC <- FALSE
                  controlAnal@param$flagBiFac   <- FALSE
                }
              } else {
                load(corConfData)
              }
              
              if (length(unique(dictVarPruebaInd$indice)) == 1) {
                controlAnal@param$flagMultiC  <- FALSE
                controlAnal@param$flagMultiNC <- FALSE
                controlAnal@param$flagBiFac   <- FALSE
              }
              
              # # Ajuste en los datos para que en el modelo no confunda los interceptos
              confkkBlockX <- confkkBlock
              codItem      <- names(confkkBlock)
              Idx <- list()
              for(ii in codItem) {
                Idx <- sub('1', names(confkkBlock), replace = 'IT1')
                names(confkkBlockX) <- Idx
              }
              
              # # Ajustes para que en el modelo no confunda los interceptos
              corConfBlockX <- corConfBlock$cor
              codItemR <- rownames(corConfBlockX)
              
              Idx <- list()
              for(ii in codItemR) {
                Idx <- paste0('IT', codItemR)
                rownames(corConfBlockX) <- Idx
                colnames(corConfBlockX) <- Idx
              }
              
              codPrueba <- dictVarPruebaInd[, 'codigo_prueba']
              jj        <- paste('P', codPrueba, 'x', sep = '')
              jj        <- unique(jj)
              
              dictVarPruebaInd[, 'codigo_pruebax'] <-
                paste('P', dictVarPruebaInd[, 'codigo_prueba'], 'x',sep =  '')
              
              dictVarPruebaInd[, 'idx'] <- paste('IT', dictVarPruebaInd[, 'id'], sep =  '')
              
              ################################################################################
              ## Run unidimensional model
              ################################################################################
              
              # # Generate unidimensional factor structure from dictionary
              blockStrucU             <- dictVarPruebaInd[, c("idx", "indice")]
              blockStrucU[, 'indice'] <- 'indice'
              names(blockStrucU)      <- c("indicator", "factor")
              
              # # Generate factor model
              unidStructureUnivBlock  <- GenerateStructure(blockStrucU)
              # # Check if any index is defined under a Rasch model and fix its factor loadings to one
              dicRasch <- subset(dictVarPruebaInd, codMod %in% codesRasch)[, c("idx", "indice")]
              names(dicRasch) <- c("indicator", "factor")
              if (nrow(dicRasch) > 0) {
                fixedStructBlock <- data.frame(dicRasch, loading = 1)
              }
              
              isRaschModel <- nrow(dicRasch) > 0
              isRasch      <- isRaschModel
              
              # # Generate factor model
              if (isRaschModel) {
                fixedModUnivBlock  <- GenerateConstrainsStructure(fixedStructBlock)
                fixStructUnivBlock <- factorStructure2LavaanModel(unidStructureUnivBlock,
                                                                  fixedLoadings = fixedModUnivBlock)
              } else {
                fixStructUnivBlock <- factorStructure2LavaanModel(unidStructureUnivBlock,
                                                                  fixedLoadings = NULL)
              }
              
              # # Run unidimensional model
              modUnidimensionalBlock <- tryCatch( cfa(fixStructUnivBlock, sample.cov = corConfBlockX,
                                                      sample.nobs = nObsConfirmatory, std.lv = TRUE,
                                                      estimator = "ML", orthogonal= FALSE),
                                                  error = function(e) "Error in solve.default"
                                                  )
              
              sumModUni <- parameterEstimates(modUnidimensionalBlock)
              outPathPbaGraph <- file.path(outPathPba,  "GRAPHS")
              if (!file.exists(outPathPbaGraph)) {
                dir.create(outPathPbaGraph)
              }
              
              outGraphUni <- file.path(outPathPbaGraph, paste("uniG_", nomKK, "_V",
                                                              versionOutput, sep = ''))
              
              grUni <- semPaths(modUnidimensionalBlock, "std", style = "OpenMx", sizeMan = 7,
                       edge.label.cex=0.8, curvePivot = TRUE, layout = "circle",
                       edge.color = "black", nCharNodes = max(nchar(names(confkkBlockX))))
              
              semPaths(modUnidimensionalBlock, "std", style = "OpenMx", sizeMan = 7,
                                edge.label.cex=0.8, curvePivot = TRUE, layout = "circle",
                                edge.color = "black", filename = outGraphUni, filetype =
                                  kExt, nCharNodes = max(nchar(names(confkkBlockX))))
              
              outFitMeasUni <- MakeSummary(modUnidimensionalBlock)
              
              ################################################################################
              # # Run multidimensional model
              ################################################################################
            
              if (controlAnal@param$flagMultiC) {
                # # Generate multidimensional factor structure from dictionary
                blockStrucM         <- dictVarPruebaInd[, c("idx", "indice")]
                names(blockStrucM)  <- c("indicator", "factor")
                multStructureBlock  <- GenerateStructure(blockStrucM)
            
                # # Check if any index is defined under a Rasch model and fix its factor loadings to one
            
                dicRasch <- subset(dictVarPruebaInd, codMod %in% codesRasch)[, c("idx", "indice")]
                names(dicRasch) <- c("indicator", "factor")
                if (nrow(dicRasch) > 0) {
                  fixedStructBlock <- data.frame(dicRasch, loading = 1)
                }
                
                isRaschModel <- nrow(dicRasch) > 0
                isRasch      <- isRaschModel
                
                # # Generate factor model
                if (isRaschModel) {
                  fixedModMultiBlock  <- GenerateConstrainsStructure(fixedStructBlock)
                  fixStructMultiBlock <- factorStructure2LavaanModel(multStructureBlock,
                                                                     fixedLoadings = fixedModMultiBlock)
                } else {
                  fixStructMultiBlock <- factorStructure2LavaanModel(multStructureBlock,
                                                                     fixedLoadings = NULL)
                }
                
                # # Run multidimensional model
                modMultidimensionalBlock <- cfa(fixStructMultiBlock,
                                                sample.cov = corConfBlockX,
                                                sample.nobs = nObsConfirmatory,
                                                std.lv = TRUE, estimator = "ML",
                                                orthogonal = FALSE)
                
                sumModMult   <- parameterEstimates(modMultidimensionalBlock)
                outGraphMult <- file.path(outPathPbaGraph, paste("MultG_", nomKK, "_",
                                                                 versionOutput, sep = ''))
                
                grMulti <- semPaths(modMultidimensionalBlock, "std", style = "OpenMx", sizeMan = 7,
                         edge.label.cex=0.8, curvePivot = TRUE, layout = "circle",
                         edge.color = "black", nCharNodes = max(nchar(names(confkkBlockX))))
                
                semPaths(modMultidimensionalBlock, "std", style = "OpenMx", sizeMan = 7,
                         edge.label.cex=0.8, curvePivot = TRUE, layout = "circle",
                         edge.color = "black", filename = outGraphMult, filetype =
                           kExt, nCharNodes = max(nchar(names(confkkBlockX))))
                
                # # Todas las medidas de ajuste
                outFitMeasMult     <- MakeSummary(modMultidimensionalBlock)
                
                # Revisar esto que esta haciendo
                #   isMultidimensional <- TRUE
                #   if  (isRaschModel) {
                #     outFitMeasMultRasch <- outFitMeasMult
                #   }
                #
                #  isUnidimensional <- isMultidimensional
              }
              
              ################################################################################
              # # Run multidimensional model without correlation between index
              ################################################################################
              # # Generate multidimensional factor structure from dictionary
            
              if(controlAnal@param$flagMultiNC) {
                blockStrucM         <- dictVarPruebaInd[, c("idx", "indice")]
                names(blockStrucM)  <- c("indicator", "factor")
                multStructureBlock  <- GenerateStructure(blockStrucM)
                # # Check if any index is defined under a Rasch model and fix its factor loadings to one
                
                dicRasch <- subset(dictVarPruebaInd, codMod %in% codesRasch)[, c("idx", "indice")]
                names(dicRasch) <- c("indicator", "factor")
                if (nrow(dicRasch) > 0) {
                  fixedStructBlock <- data.frame(dicRasch, loading = 1)
                }
                
                isRaschModel <- nrow(dicRasch) > 0
                isRasch      <- isRaschModel
                
                # # Generate factor model
                if (isRaschModel) {
                  fixedModMultiBlock    <- GenerateConstrainsStructure(fixedStructBlock)
                  fixStructMultiBlockNR <- factorStructure2LavaanModel(multStructureBlock,
                                                                       fixedLoadings = fixedModMultiBlock)
                } else {
                  fixStructMultiBlockNR <- factorStructure2LavaanModel(multStructureBlock,
                                                                       fixedLoadings = NULL)
                }
                
                # # Run multidimensional model
                
                modMultidimensionalBlockNR <- cfa(fixStructMultiBlockNR,
                                                  sample.cov = corConfBlockX,
                                                  sample.nobs = nObsConfirmatory,
                                                  std.lv = TRUE, estimator = "ML",
                                                  orthogonal = TRUE)
                
                sumModMultNR <- parameterEstimates(modMultidimensionalBlockNR)
                
                outGraphMultNR <- file.path(outPathPbaGraph, paste("MultNCG_", nomKK, "_",
                                                                   versionOutput, sep = ''))
                
                grMultiNC <- semPaths(modMultidimensionalBlockNR, "std", style = "OpenMx", sizeMan = 7,
                             edge.label.cex=0.8, curvePivot = TRUE, layout = "circle",
                             edge.color = "black", nCharNodes = max(nchar(names(confkkBlockX))))
                
                semPaths(modMultidimensionalBlockNR, "std", style = "OpenMx", sizeMan = 7,
                         edge.label.cex=0.8, curvePivot = TRUE, layout = "circle",
                         edge.color = "black", filename = outGraphMultNR, filetype =
                           kExt, nCharNodes = max(nchar(names(confkkBlockX))))
                
                # # Todas las medidas de ajuste
                
                outFitMeasMultNR  <- MakeSummary(modMultidimensionalBlockNR)
                
                #    isMultidimensional <- TRUE
                #
                #    if  (isRaschModel) {
                #      outFitMeasMultRasch <- outFitMeasMultNR
                #    }
                }
                ################################################################################
                # # Run bifactorial model
                ################################################################################
                # # Generate unidimensional factor structure from dictionary
                if (controlAnal@param$flagBiFac) {
                  blockStrucM         <- dictVarPruebaInd[, c("idx", "indice")]
                  names(blockStrucM)  <- c("indicator", "factor")
                  multStructureBlock  <- GenerateStructure(blockStrucM)
                  
                  # # Check if any index is defined under a Rasch model and fix its factor loadings to one
                  dicRasch <- subset(dictVarPruebaInd, codMod %in% codesRasch)[, c("idx", "indice")]
                  names(dicRasch) <- c("indicator", "factor")
                  if (nrow(dicRasch) > 0) {
                    fixedStructBlock <- data.frame(dicRasch, loading = 1)
                  }
                  
                  isRaschModel <- nrow(dicRasch) > 0
                  isRasch      <- isRaschModel
                  
                  # # Generate factor model
                  
                  if (isRaschModel) {
                    fixedModBifaBlock <- GenerateConstrainsStructure(fixedStructBlock)
                    fixStructBifaBlock <- factorStructure2LavaanModel(multStructureBlock,
                                                                      fixedLoadings = fixedModBifaBlock)
                  } else {
                    fixStructBifaBlock <- factorStructure2LavaanModel(multStructureBlock,
                                                                      fixedLoadings = NULL)
                  }
                  
                  lenghtIndices       <- length(fixStructBifaBlock)
                  
                  fixStructBifaBlock[lenghtIndices+1] <- paste(jj , "=~",
                                                               paste(names(multStructureBlock),
                                                                     collapse = " + "))
                  
                  # # Run bifactoraial model
                  modBifactorialBlock <- cfa(fixStructBifaBlock, sample.cov = corConfBlockX,
                                             sample.nobs = nObsConfirmatory,
                                             std.lv = TRUE, estimator = "ML",
                                             orthogonal = TRUE)
                  
                  sumModBifa <- parameterEstimates(modBifactorialBlock)
                  outGraphBifa <- file.path(outPathPbaGraph, paste("bifaG_", nomKK, "_",
                                                                   versionOutput, sep = ''))
                  
                  grapBiFa <- try(semPaths(modBifactorialBlock, "std", edge.label.cex=0.5,
                                           curvePivot = FALSE, style = "lisrel", rotation = 4,
                                           layout = "tree2", edge.color = "black",
                                           optimizeLatRes = FALSE, sizeLat = 5,
                                           filetype = kExt, intStyle = "single",
                                           arrowAngle = pi/16, sizeMan = 5,
                                           filename = outGraphBifa,
                                           nCharNodes =
                                             max(nchar(names(confkkBlockX)))))
                  
                  # # Todas las medidas de ajuste
                  outFitMeasBifa <- try(MakeSummary(modBifactorialBlock))
                  if (class(outFitMeasBifa) == "try-error") {
                    controlAnal@param$flagBiFac <- FALSE
                  } 
                }
              
              resulConf[[kk]] <- list('grUni' = c('dir' = outGraphUni, 'gr' = grUni),
                                      'grMulti' = ifelse(controlAnal@param$flagMultiC, 
                                                         c('dir' = outGraphMult, 'gr' = grMulti), NA),
                                      'grMultiNC' = ifelse(controlAnal@param$flagMultiNC, c('dir' = outGraphMultNC, 'gr' = grMultiNC), NA),
                                      'grBifa' = ifelse(controlAnal@param$flagBiFac, c('dir' = outGraphBifa, 'gr' = grapBiFa), NA))
              
              ########################################################################
              # # Salidas en Excel
              ########################################################################
              
              wb <<- createWorkbook()
              
              # # cell style
              # # header style
              
              csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) +
                Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
              
              # # percentages style
              csPor <- CellStyle(wb) + DataFormat("0.0%")
              # # estilo de columnas que reportan la desviación estándar del por
              csDs <- CellStyle(wb) + DataFormat("(0.0%)") +
                Alignment(v = "VERTICAL_CENTER") + Border()
              csDsE <- CellStyle(wb) + Alignment(v = "VERTICAL_CENTER", wrapText = TRUE) +
                Border()
              
              # # estilo de columnas que reportan n
              csN <- CellStyle(wb) + DataFormat("0.000") + Font(wb, isItalic = TRUE)
              csNE <- CellStyle(wb) + Font(wb, isItalic = TRUE)
              # # borde
              csPC <- CellStyle(wb) + Border() +
                Alignment(v = "VERTICAL_CENTER", wrapText = TRUE)
              # # fuente en negrilla
              csNeg <- CellStyle(wb) + Font(wb, isBold = TRUE)
              # # Columna centrada
              cen  <- CellStyle(wb) + Alignment(h="ALIGN_CENTER")
              
              namesSheet <- c('Items', 'Unidimensional', 'Multidimensional',
                              'MultidimensionalNC', 'Bifactorial', 'Fits')
              
              # # Salidas
              
              if(controlAnal@param$flagMultiC | controlAnal@param$flagMultiNC) {
                summarys   <- list(sumModUni = sumModUni, sumModMult = sumModMult,
                                   sumModMultNR = sumModMultNR)
                
                outGraphs  <- c(outGraphUni, outGraphMult, outGraphMultNR)
                
                outFits  <- data.frame(outFitMeasUni = outFitMeasUni,
                                       outFitMeasMult   = outFitMeasMult,
                                       outFitMeasMultNR = outFitMeasMultNR)
                
                CreateExcel(namesSheet[1],  summarys, outGraphs, outFits, model = FALSE,
                            items = TRUE, outfit = FALSE)
                mapply(CreateExcel, namesSheet[2:4],  summarys, outGraphs, outFits,
                       model = TRUE, items = FALSE, outfit = FALSE)
              }
              
              if(controlAnal@param$flagBiFac) {
                mapply(CreateExcel, namesSheet[5],  list(sumModBifa = sumModBifa),
                       c(outGraphBifa), data.frame(outFitMeasBifa = outFitMeasBifa),
                       model = TRUE, items = FALSE, outfit = FALSE)
              }
              
              if(controlAnal@param$flagMultiC | controlAnal@param$flagMultiNC) {
                if(controlAnal@param$flagBiFac) {
                  outFits <- data.frame(outFits, outFitMeasBifa = outFitMeasBifa)
                }
                
                CreateExcel(namesSheet[6],  summarys, outGraphs, outFits, model = FALSE,
                            items = FALSE, outfit = TRUE)
              }
              
              if (!controlAnal@param$flagMultiC & !controlAnal@param$flagMultiNC & !controlAnal@param$flagBiFac) {
                # # Creacion de Excel en caso que solo aplique el univariado
                CreateExcel(namesSheet[1], sumModUni, outGraphUni, outFitMeasUni,
                            model = FALSE, items = TRUE, outfit = FALSE)
                
                CreateExcel(namesSheet[2],  sumModUni, outGraphUni, outFitMeasUni,
                            model = TRUE, items = FALSE, outfit = FALSE)
                
                CreateExcel(namesSheet[6],  sumModUni, outGraphUni, outFitMeasUni,
                            model = FALSE, items = FALSE, outfit = TRUE)
              }
              
              outFile <- file.path(outPathPba,
                                   paste("05Confirmatorio_", nomKK,"_V", versionOutput,
                                         ".xlsx", sep = ''))
              if (file.exists(outFile)) {
                file.remove(outFile)
              }
              
              saveWorkbook(wb, file = outFile)
              cat("..... Salida en ", outFile, '\n')
              rm(wb, dictVarPruebaInd)
              object@Analisis[[codeName]] <- controlAnal
             # return(object)
            }
          }
         
    )

################################################################################
# # Apply the confirmatory function to each test in controlData
################################################################################


for (prueba in names(controlData)) {
  print(prueba)
  controlData[[prueba]] <- confirmatAnalysis(controlData[[prueba]])
}

save(controlData, file = file.path(inPath, controlFile))