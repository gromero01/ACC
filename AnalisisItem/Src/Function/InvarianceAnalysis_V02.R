################################################################################
# # fitIRTIndices.Rnw
# # R Versions: R version 2.14.0
# #
# # Author(s): Fabio Tejedor, based on 
# #            victor's procedure
# #
# #
# # Process: SABER 5° y 9° Citizenship competencies 
# # Description: Generate the xlsx with tables to analize invarianceby
# # classification variable
# #
# # Inputs: data set with test PBA
# #
# # Outputs: xlsx file with tables from invariance separate and join 
# #
# # File history:
# #   20140315
################################################################################
 
InvarianceAnalysis <- function(confkkBlock   , blockStrucUbyIn, 
                               listVaInterest, toInvarFolder,
                               dictVarPruebaIndSub){  
    # # description return xlsx file with fitting measures in invariance
    # # Analysis : one by each one as independent fitting and
    # # considering the groups inside the estimation process
    # #
    # # Arg: 
    # #     confkkBlock    [data.frame] dataframe with variables 
    # #                                 and variables of grouping
    # #     listVaInterest [character]  with variables of grouping , 
    # #                                 those must be 
    # #                                 in the data.frame kkBlock. 
    # #     blockStrucUbyIn[data.frame] data frame with indicator and
    # #                                 factor realted to items and name 
    # #                                 of index. The last one must
    # #                                 be repeated as well as number of items 
    # #     toInvarFolder ["character"] route where correlation matrices
    # #                                 will save
    # #     dictVarPruebaIndSub[data.frame] subset of dictionary where
    # #                                     query is defined by indice 
    # #
    # # Ret:
    # #  xlsx format file with tables of invariance analysis 

  # # temporary object, save the results of printing tables
  tempTableInvar <- paste("outTabInvar_", listVaInterest, sep ="") 
  
  # # index that you are running 
  index <- as.character(unique(blockStrucUbyIn[, "factor"]))

  routeTemp <- "../../../../src"
  setwd(toInvarFolder)
  blockItems <- substr(blockStrucUbyIn[, "indicator"], 2, 99)
  for(jj in 1:length(listVaInterest)){
    
  tempTableInvarSave <- list()    
  vaInterest <- listVaInterest[jj]
  invariBlock    <- confkkBlock[, c(blockItems, vaInterest)]

  # # Split the data set by vaInterest
  invariBlock <- split(invariBlock, f = invariBlock[, vaInterest])  

  corList <- list()
  catElim <- NULL
  for (zz in names(invariBlock)) {
    invariBlock[[zz]] <- invariBlock[[zz]][, colnames(invariBlock[[zz]]) 
                                             != vaInterest]
    if (zz %in% catElim) {
     invariBlock[[zz]] <- NULL
    } else {
      # # Obtain correlation matrix
      corData <- paste("corIndice",index, "_",vaInterest, zz, "_V", 
                       verDataIn,  ".RData", sep = "")
      if (!file.exists(corData)) {
        corList[[zz]] <- try(hetcor(invariBlock[[zz]], pd = TRUE, 
                                   use = useCor, std.err = FALSE, 
                                   ML = FALSE))$cor
        rownames(corList[[zz]]) <- substr(rownames(corList[[zz]]), 1, 99)
        colnames(corList[[zz]]) <- substr(colnames(corList[[zz]]), 1, 99)        
        if (class(corList[[zz]]) != "try-error") {  
        name <- paste("corIndice",index, "_",vaInterest, 
                      zz, "_V", verDataIn,  sep = "")
          assign( name , corList[[zz]] )
          save(list=name, file = corData)
        } else {
          cat("No se estimo la matriz policórica inicial\n\n")
        }
     } else {
        name <- paste("corIndice",index, "_",vaInterest, 
                      zz, "_V", verDataIn,  sep = "")
        load(corData)
        corList[[zz]] <- get(name)
      }    
    }
  rownames(corList[[zz]]) <- paste("V", rownames(corList[[zz]]), sep = "")
  colnames(corList[[zz]]) <- paste("V", colnames(corList[[zz]]), sep = "")              
  }
  nObsBlockI <- lapply(invariBlock, nrow)

################################################################################
# # analysis by grado separetely goodnes of fit 
################################################################################
  blockStrucUbyIn <- blockStrucUbyIn[,-3]
  unidStructureUnivBlock <- GenerateStructure(blockStrucUbyIn)
  fixStructUnivBlock     <- factorStructure2LavaanModel(unidStructureUnivBlock,
                                                        fixedLoadings = NULL)

  cfaSepar <- paste("cfaSepar", 1:length(corList), sep ="")
  tabSumSepar <- data.frame(rep(NA, 22))
  for (ii in seq(length(cfaSepar))) {
    temp <- lavaan:::cfa(fixStructUnivBlock, sample.cov = corList[[ii]], 
                            std.lv = TRUE  ,sample.nobs = nObsBlockI[[ii]],
                            estimator = "ML", orthogonal = FALSE)
    sumMeas <- tryCatch(MakeSummary(temp), 
                        error = function(e) "No Converge")
    if(sumMeas == "No Converge") {
       sumTemp <- data.frame(rep("No es posible calcular medidas de ajus :(",22))
       colnames(sumTemp) <- names(corList[ii])
       tabSumSepar <- data.frame(tabSumSepar, sumTemp)
         }
    else {
     sumTemp <- data.frame(sumMeas)
     colnames(sumTemp) <- names(corList[ii])
     if(ii !=1) { 
          rownames(tabSumSepar) <- rownames(sumTemp)
          tabSumSepar <- data.frame(tabSumSepar, sumTemp)
        }
     else tabSumSepar <- sumTemp
      }
   }
    nobser <- unlist(nObsBlockI)
    tempTableInvarSave[[1]] <- rbind(tabSumSepar, nObs =nobser)
 
################################################################################
# # continue joint analysis 
################################################################################   
  
  res       <- list()
  
  res$fit.configural <- lavaan:::cfa(fixStructUnivBlock, sample.cov = corList, 
                            sample.nobs = nObsBlockI, std.lv = TRUE, 
                            estimator = "ML", orthogonal = FALSE, 
                            group.equal = "")
  res$fit.loadings   <- lavaan:::cfa(fixStructUnivBlock, sample.cov = corList, 
                            sample.nobs = nObsBlockI, std.lv = TRUE, 
                            estimator = "ML", orthogonal = FALSE, 
                            group.equal = c("loadings")) # # metric
  res$fit.intercepts <- lavaan:::cfa(fixStructUnivBlock, sample.cov = corList, 
                            sample.nobs = nObsBlockI, std.lv = TRUE, 
                            estimator = "ML", orthogonal = FALSE, 
                            group.equal = c("loadings","intercepts"))
  res$fit.residuals <- lavaan:::cfa(fixStructUnivBlock, sample.cov = corList, 
                            sample.nobs = nObsBlockI, std.lv = TRUE, 
                            estimator = "ML", orthogonal = FALSE, 
                            group.equal = c("loadings",
                                           "intercepts", "residuals"))
  res$fit.varianceFa <- lavaan:::cfa(fixStructUnivBlock, sample.cov = corList, 
                            sample.nobs = nObsBlockI, std.lv = TRUE, 
                            estimator = "ML", orthogonal = FALSE, 
                            group.equal = c("loadings",
                                           "intercepts", "residuals",
                                           "lv.variances"))

  res$fit.covarianceFa <- lavaan:::cfa(fixStructUnivBlock, sample.cov = corList, 
                            sample.nobs = nObsBlockI, std.lv = TRUE, 
                            estimator = "ML", orthogonal = FALSE, 
                            group.equal = c("loadings",
                                           "intercepts", "residuals",
                                           "lv.covariances"))

  res$fit.means <- cfa(fixStructUnivBlock, sample.cov = corList, 
                      sample.nobs = nObsBlockI, std.lv = TRUE, 
                      estimator = "ML", orthogonal = FALSE, 
                      group.equal = c("loadings", 
                                            "intercepts", "residuals",
                                            "means"))

  outFitInvar        <- lapply(res, 
              function(x){
              tryCatch(MakeSummary(x),  
              error = function(e) "No es posible calcular medidas de ajus :(")})
 
  namess <- rownames(outFitInvar[[1]])
  outFitMeasInvar <- data.frame(outFitInvar)
  colnames(outFitMeasInvar) <- names(outFitInvar)
  outFitMeasInvar[, "rangeConfLoadInter"] <- NA
  toRange <- which(rownames(outFitMeasInvar) %in% c("CFI" , "RMSEA"   ,
                                                    "SRMR", "NFI"     ,
                                                    "IFI" , "GAMMAHAT",
                                                    "MCDONALD"))
 outFitMeasInvar[toRange, "rangeConfLoadInter"] <- 
   apply(outFitMeasInvar[toRange, 1:3], 1, 
                 function(x){
                         if(class(x)!= "numeric") return(NA)  
                         else {y <- range(x)
                               z <- y[2] - y[1]
                               return(z)
                         }
                        })

  tempTableInvarSave[[2]] <- outFitMeasInvar
  names(tempTableInvarSave) <- c("Separate", "Join")

  assign(tempTableInvar[jj] , tempTableInvarSave)
 } ## end of for to invariance analysis

  
setwd(routeTemp)

# # information data part   

namesPrueba <- subset(dictVarPruebaIndSub, codigo_prueba == kk, 
                       select = c(codigo_prueba, prueba))
namesPrueba <- namesPrueba [1, ]
namesPrueba[, 'índice'] <- index
namesPrueba[, 'nItems'] <- nrow(blockStrucUbyIn)
namesPrueba[, 'Nota'] <- c("La columna rangeConfLoadInter es el rango entre las columnas configural, loadings e intercepts para CFI, RMSEA, SRMR, NFI, IFI, GAMMAHAT, MCDONALD")
namesPrueba <- t(namesPrueba)


################################################################################
# # output .xlsx file with data tables of results of invariance analysis
################################################################################

#dir.create(file.path(outPath, "Invariance07"), showWarnings = FALSE)
outFile <-  file.path(outPathPba, paste("Invarianza", index, ".xlsx", sep = ""))

################################################################################
# # Styles to exportation
################################################################################

# # estilo de las celdas
  # # estilo del encabezado
 wb <- createWorkbook()
  csEncOne <- CellStyle(wb) + Font(wb, isBold = TRUE) +
            Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
  csEncTwo <- CellStyle(wb) + Font(wb, isBold = TRUE, 
                                   heightInPoints = 15, 
                                   color = rainbow(10)[8]) +
              Alignment(h = "ALIGN_CENTER")
  # # estilo de longitud decimal 
  csDec1 <- CellStyle(wb) + DataFormat("#,##0.00") + Font(wb, isItalic = FALSE) +
           Border(pen = "BORDER_DOUBLE", position = "LEFT")

  csDec2 <- CellStyle(wb) + DataFormat("#,##0.00") + Font(wb, isItalic = FALSE)
  # # fuente en negrilla
  csNeg <- CellStyle(wb) + Font(wb, isBold = TRUE)
################################################################################
################################################################################
 sheetOut <- c("Indices", paste("sheetOut",listVaInterest, sep = "_"))


 assign(sheetOut[1], createSheet(wb, sheetName = "Information")) 

  # # # information for indices .... Model with indices
  infoTable <- list()
  infoTable$strucInd <- blockStrucUbyIn
  infoTable$strucInd[, "indicator"] <- substr(infoTable$strucInd[,"indicator"],2,99)
  colnames(infoTable$strucInd) <- c("Item", "Index")

  infoTable$infoPBA <- rbind(
                        unique(dictVarPrueba[, "prueba"]),
                        unique(dictVarPrueba[, "codigo_prueba"]))
  rownames(infoTable$infoPBA) <- c("Prueba", "Código_Prueba")

  addDataFrame(infoTable$strucInd, sheet  = get(sheetOut[1]),
              startRow = 8, startColumn = 1, row.names = FALSE,
              col.names = TRUE, colnamesStyle = csEncOne)

# # information datatable  

   addDataFrame(namesPrueba[-5,], sheet  = get(sheetOut[1]),
              startRow = 1, startColumn = 1, row.names = TRUE,
              col.names = FALSE, colnamesStyle = csEncOne,
              rownamesStyle = csNeg) 

### sheet by group   
 for(jj in 1:length(tempTableInvar)){
   assign(sheetOut[jj+1], createSheet(wb, sheetName = listVaInterest[jj])) 

# # An'alisis separado 
   addDataFrame(data.frame("Análisis separado"), sheet  = get(sheetOut[jj+1]),
              startRow = 8, startColumn = 1, row.names = FALSE,
              col.names = FALSE, 
              colStyle = list('1' = csEncTwo))
 

  addDataFrame(get(tempTableInvar[jj])$Separate, sheet  = get(sheetOut[jj+1]),
              startRow = 12, startColumn = 1, row.names = TRUE,
              col.names = TRUE, colnamesStyle = csEncOne,
              colStyle = list('1' = csDec1, '2' = csDec2),
              rownamesStyle = csNeg) 
  ncols <- ncol(get(tempTableInvar[jj])$Separate)

  setColumnWidth(get(sheetOut[jj+1]), 1, 22)
  setColumnWidth(get(sheetOut[jj+1]), seq(ncols)+1, 15)  


# # An'alisis conjunto 
  addDataFrame(data.frame("Análisis conjunto"), sheet  = get(sheetOut[jj+1]),
              startRow = 8, startColumn = ncols + 4, row.names = FALSE,
              col.names = FALSE, 
              colStyle = list('1' = csEncTwo))

  addDataFrame(get(tempTableInvar[jj])$Join, sheet  = get(sheetOut[jj+1]),
              startRow = 12, startColumn = ncols + 4, row.names = TRUE,
              col.names = TRUE, colnamesStyle = csEncOne,
              colStyle = list('1' = csDec1, '2' = csDec2),
              rownamesStyle = csNeg) 

  setColumnWidth(get(sheetOut[jj+1]), ncols + 4, 22)
  setColumnWidth(get(sheetOut[jj+1]), ncols + 4 + (1:8), 22)  


# # information datatable  

   addDataFrame(namesPrueba, sheet  = get(sheetOut[jj+1]),
              startRow = 1, startColumn = 1, row.names = TRUE,
              col.names = FALSE, colnamesStyle = csEncOne,
              rownamesStyle = csNeg) 

  }
  saveWorkbook(wb, file = outFile) 
  rm(list = tempTableInvar)
}
