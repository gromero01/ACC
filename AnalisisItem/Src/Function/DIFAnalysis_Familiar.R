################################################################################
# # fitIRTIndices.Rnw
# # R Versions: R version 2.14.0
# #
# # Author(s): Fabio Tejedor, based on 
# #            victor's procedure
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
 
DIFAnalysis <- function(confkkBlock   , blockStrucUbyIn, 
                        listVaInterest, dictVarPruebaIndSub,
                        versionOutput){ 

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
    # #     dictVarPruebaIndSub[data.frame] subset of dictionary where
    # #                                     query is defined by indice 
    # #
    # # Ret:
    # #  xlsx format file with tables of invariance analysis 

  # # temporary object, save the results of printing tables
  tempTableDIF <- paste("outTabDIF_", listVaInterest, sep ="") 
  cutoffDIFF   <- paste("cutDIF_", listVaInterest, sep ="") 
  
  # # index that you are running 
  index <- as.character(unique(blockStrucUbyIn[, "factor"]))

  blockItems <- substr(blockStrucUbyIn[, "indicator"], 2, 99)
  # # load cut off points of DIF 
  # # # cut off points for DRU
  cutDRU <- cutoffDIF$DRU

  # # # cut off points for DRN
  cutDRN <- cutoffDIF$DRN

  for(jj in 1:length(listVaInterest)){
      tempTableInvarSave <- list()   
      cutoffs            <- list()
      vaInterest   <- listVaInterest[jj]
      indicator    <- blockStrucUbyIn[, "indicator"]
      twoPL        <- ifelse(dictVarPruebaIndSub[, "codMod"] == "05", 1, 0)
      indicadorDIC <- dictVarPruebaIndSub[twoPL == 1, "id"]
      indicadorPOL <- dictVarPruebaIndSub[twoPL == 0, "id"]

    ################################################################################
    # # to compute cut off to DIF analysisfrom table for dochotomous
    ################################################################################
      assign(cutoffDIFF[jj], list())
      prop     <- sort(table(confkkBlock[, vaInterest]), decreasing = TRUE)
      propC    <- prop[1]
      ratioCat <- sort(round(propC/prop), decreasing = TRUE)[1]
      nObs     <- nrow(confkkBlock) 
      # # para DRU 
      u1 <- unique(cutDRU[, "SizeSam"])
      v1 <- abs(nObs - u1)
      w1 <- u1[which.min(v1)]

      # # para DRN  
      u2 <- unique(cutDRN[, "RatioSample"])
      v2 <- abs(ratioCat - u2)
      w2 <- u2[which.min(v2)]  

      cutoffDRU  <- subset(cutDRU, SizeSam == w1 & RatioSample == w2, 
                    select = c(cutBigDIF, cutSmallDIF))

      cutoffDRN <- subset(cutDRN, SizeSam == w1 & RatioSample == w2, 
                    select = c(cutBigDIF, cutSmallDIF))
      
      cutOffdifDic <- rbind(DRU = cutoffDRU, DRN = cutoffDRN)
      cutOffdifPol <- cutOffdifDic
      cutOffdifPol[ , ] <- 0.02

      temp <- get(cutoffDIFF[jj])

      temp$DIC <- cutOffdifDic
      temp$POL <- cutOffdifPol

      assign(cutoffDIFF[jj], temp)
      rm(u1, u2, v1, v2, w1, w2, temp)

    ################################################################################
    # # start DIF analysis
    ################################################################################

    ################################################################################
      # # dicot'omicos
    ################################################################################
       assign(tempTableDIF[jj], list())
       temp     <- get(tempTableDIF[jj])       
       temp$DIC <- NA
       temp$POL <- NA
       temp 
      if(length(indicadorDIC)!=0){
          datatoDicDif <- data.frame(lapply(confkkBlock[, indicadorDIC],as.numeric)) 
          datatoDicDif <- datatoDicDif - 1 
          grouptoDif   <- as.numeric(confkkBlock[, vaInterest])
          sink(file = "borrar.txt")
          difDic <- func_RL_ambas_fases(datatoDicDif,grouptoDif,
                                        pseudo.R = "McFadden", tipo = "Separado",
                                        use.glm = TRUE, use.nnet = FALSE)
          sink();unlink("borrar.txt")
          temp$DIC <- difDic
          }

      ################################################################################
      # # polit'omicos
      ################################################################################
      if(length(indicadorPOL)!=0){
          datatoPolDif <- data.frame(lapply(confkkBlock[, indicadorPOL],as.numeric))
          datatoPolDif <- data.frame(do.call("cbind", datatoPolDif))
          grouptoDif       <- as.numeric(confkkBlock[, vaInterest])  
          sink(file = "borrar.txt")
          difPol <- func_RL_ambas_fases(datatoPolDif, grouptoDif,
                                        pseudo.R = "McFadden", tipo = "Separado",
                                        use.glm = FALSE, use.nnet = TRUE)
          sink();unlink("borrar.txt")
          temp$POL <- difPol
        }

      assign(tempTableDIF[jj], temp)
      rm(temp)

    ################################################################################
    # # to test if the items show DIF 
    ################################################################################

      tempDIF <- get(tempTableDIF[jj])
      tempCUT <- get(cutoffDIFF[jj])
      if(class(tempDIF$DIC)== "data.frame") {

       tempDIF$DIC <- round(tempDIF$DIC, 5)
       tempDIF$DIC["DIF_UNIFORME_SMALL", ] <- 
         ifelse(tempDIF$DIC["DRU", ] > tempCUT$DIC["DRU", "cutSmallDIF"], 
                "Difiere", NA)

      tempDIF$DIC["DIF_UNIFORME_BIG", ] <- 
         ifelse(tempDIF$DIC["DRU", ] > tempCUT$DIC["DRU", "cutBigDIF"], 
                "Difiere", NA)                                                    

      tempDIF$DIC["DIF_UNIFORME", ] <- 
        ifelse(tempDIF$DIC["DIF_UNIFORME_SMALL", ] == "Difiere" |
               tempDIF$DIC["DIF_UNIFORME_BIG", ]   == "Difiere", "Difiere",NA )

      tempDIF$DIC["DIF_NO_UNIFORME_SMALL", ] <- 
         ifelse(tempDIF$DIC["DRN", ] > tempCUT$DIC["DRN", "cutSmallDIF"], 
                "Difiere", NA)

      tempDIF$DIC["DIF_NO_UNIFORME_BIG", ] <- 
         ifelse(tempDIF$DIC["DRN", ] > tempCUT$DIC["DRN", "cutBigDIF"], 
                "Difiere", NA)  

      tempDIF$DIC["DIF_NO_UNIFORME", ] <- 
        ifelse(tempDIF$DIC["DIF_NO_UNIFORME_SMALL", ] == "Difiere" |
               tempDIF$DIC["DIF_NO_UNIFORME_BIG", ]   == "Difiere", "Difiere",NA )

      }

      if(class(tempDIF$POL) == "list"){
       tempDIF$POL$testDIF <- round(tempDIF$POL$testDIF, 5)
       tempDIF$POL$testDIF["DIF_UNIFORME_SMALL", ] <- 
         ifelse(tempDIF$POL$testDIF["DRU", ] > tempCUT$POL["DRU", "cutSmallDIF"], 
                "Difiere", NA)

      tempDIF$POL$testDIF["DIF_UNIFORME_BIG", ] <- 
         ifelse(tempDIF$POL$testDIF["DRU", ] > tempCUT$POL["DRU", "cutBigDIF"], 
                "Difiere", NA)      

      tempDIF$POL$testDIF["DIF_UNIFORME", ] <- 
        ifelse(tempDIF$POL$testDIF["DIF_UNIFORME_SMALL", ] == "Difiere" |
               tempDIF$POL$testDIF["DIF_UNIFORME_BIG", ]   == "Difiere", "Difiere",NA )


      tempDIF$POL$testDIF["DIF_NO_UNIFORME_SMALL", ] <- 
         ifelse(tempDIF$POL$testDIF["DRN", ] > tempCUT$POL["DRN", "cutSmallDIF"], 
                "Difiere", NA)

      tempDIF$POL$testDIF["DIF_NO_UNIFORME_BIG", ] <- 
         ifelse(tempDIF$POL$testDIF["DRN", ] > tempCUT$POL["DRN", "cutBigDIF"], 
                "Difiere", NA)   
        
      tempDIF$POL$testDIF["DIF_NO_UNIFORME", ] <- 
       ifelse(tempDIF$POL$testDIF["DIF_NO_UNIFORME_SMALL", ] == "Difiere" |
              tempDIF$POL$testDIF["DIF_NO_UNIFORME_BIG", ]   == "Difiere", "Difiere",NA )

      }
      assign(tempTableDIF[jj], tempDIF)
      rm(tempDIF, tempCUT)
  } ## end of for DIF analysis

#setwd(routeTemp)
# # information data part   

namesPrueba <- subset(dictVarPruebaIndSub, codigo_prueba == kk, 
                       select = c(codigo_prueba, prueba))
namesPrueba <- namesPrueba [1, ]
namesPrueba[, 'índice'] <- index
namesPrueba[, 'nItemsDic'] <- length(indicadorDIC)
namesPrueba[, 'nItemsPol'] <- length(indicadorPOL)
namesPrueba[, 'nItems'] <- length(indicadorDIC) + length(indicadorPOL)
namesPrueba <- t(namesPrueba)
 
  
################################################################################
# # output .xlsx file with data tables of results of invariance analysis
################################################################################

#dir.create(file.path(outPath, "Invariance07"), showWarnings = FALSE)
outFile <-  file.path(outPathPba, paste("DIF_", index, "_" ,
                                        versionOutput, ".xlsx", sep = ""))
################################################################################
# # Styles to exportation
################################################################################

# # estilo de las celdas
  # # estilo del encabezado
 wb <- createWorkbook()
  csEncOne  <- CellStyle(wb) + Font(wb, isBold = TRUE) +
            Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
  csEncTwo  <- CellStyle(wb) + Font(wb, isBold = TRUE, 
                                   heightInPoints = 15, 
                                   color = rainbow(10)[8]) +
              Alignment(h = "ALIGN_LEFT")

  csEncThree <- CellStyle(wb) + Font(wb, isBold = TRUE) +
            Border(position = c("BOTTOM", "LEFT", "TOP", "RIGHT"),
                   pen = "BORDER_THIN") + 
            Alignment(h = "ALIGN_CENTER") 

  csEncFour <- CellStyle(wb) + Font(wb, isBold = FALSE) +
            Border(position = c("BOTTOM", "LEFT", "TOP", "RIGHT"),
                   pen = "BORDER_THIN") + 
            Alignment(h = "ALIGN_CENTER") 

  csEncFive  <- CellStyle(wb) + Font(wb, isBold = TRUE, 
                                   heightInPoints = 12, 
                                   color = "red") +
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

 addDataFrame(namesPrueba, sheet  = get(sheetOut[1]),
              startRow = 1, startColumn = 1, row.names = TRUE,
              col.names = FALSE, colnamesStyle = csEncOne,
              rownamesStyle = csNeg)
  setColumnWidth(get(sheetOut[1]), 1, 15)
### sheet by group   
 for(jj in 1:length(tempTableDIF)){
   assign(sheetOut[jj+1], createSheet(wb, sheetName = listVaInterest[jj])) 

# # An'alisis separado 
 
   tempDIC <- get(tempTableDIF[jj])$DIC  
   if(length(get(tempTableDIF[jj])$POL) == 1) {tempPOL <- NA }else{
      tempPOL <- get(tempTableDIF[jj])$POL$testDIF
   }

# # Para dicot'omicos
  if(length(tempDIC) == 1) {
    tempDIC      <- data.frame("No hay ítems dicotómicos")
    rownamedic      <- FALSE
    colnamedic      <- FALSE
    colnamStyledic  <- NULL
    colStyledic     <- NULL
    rowStyledic     <- NULL
    } else {
    rowTempOne <- c("G1", "p.value1", "G2", "p.value2", 
                    "R1", "R2", "R3", "DRU", "DRN")
    rowTempTwo <- c("DIF_UNIFORME_SMALL", "DIF_UNIFORME_BIG", 
                    "DIF_UNIFORME", "DIF_NO_UNIFORME_SMALL", 
                    "DIF_NO_UNIFORME_BIG", "DIF_NO_UNIFORME") 
    tempDICone        <- data.frame(lapply(tempDIC[rowTempOne, ], as.numeric))
    tempDICtwo        <- tempDIC[rowTempTwo, ]    
    colnames(tempDICone) <- substr(colnames(tempDICone), 2, 99)
    colnames(tempDICtwo) <- substr(colnames(tempDICtwo), 2, 99)        
    rownames(tempDICone) <- rowTempOne
    rownames(tempDICtwo) <- rowTempTwo
    rownamedic        <- TRUE
    colnamedic        <- TRUE 
    colnamStyledic    <- csEncOne
    rowStyledic       <- csNeg
    # # column's style for test two
    colStyleTwoDIC <- list()
    for(zz in 1:ncol(tempDICtwo)){
        if(!all(is.na(tempDICtwo[, zz]))){ 
          colStyleTwoDIC[[zz]] <- csEncFive
        } else colStyleTwoDIC[[zz]] <- NULL
    }
    
    if(length(colStyleTwoDIC) > 0){
      names(colStyleTwoDIC) <- as.character(1:length(colStyleTwoDIC))
    }

  }

# # Para polit'omicos 
  if(length(tempPOL) == 1) {
    tempPOL <- "No hay ítems politómicos "
    rownamepol      <- FALSE
    colnamepol      <- FALSE
    colnamStylepol <- NULL
    colStylepol     <- NULL
    rowStylepol     <- NULL
    } else {
    rowTempOne        <- c("G1", "p.value1", "G2", "p.value2", 
                           "R1", "R2", "R3", "DRU", "DRN")
    rowTempTwo        <- rownames(tempPOL)[!rownames(tempPOL)%in%rowTempOne]
    tempPOLone        <- data.frame(lapply(tempPOL[rowTempOne, ], as.numeric))
    tempPOLtwo        <- tempPOL[rowTempTwo, ]        
    colnames(tempPOLone) <- substr(colnames(tempPOLone), 2, 99) 
    colnames(tempPOLtwo) <- substr(colnames(tempPOLtwo), 2, 99)     
    rownames(tempPOLone) <- rowTempOne
    rownames(tempPOLtwo) <- rowTempTwo    
    rownamepol           <- TRUE
    colnamepol           <- TRUE 
    colnamaStylepol      <- csEncOne
    rowStylepol          <- csNeg
    # # column's style for test two
    colStylePOL <- list()
    for(zz in 1:ncol(tempPOLtwo)){
        if(!all(is.na(tempPOLtwo[, zz]))){ 
          colStylePOL[[zz]] <- csEncFive
        } else colStylePOL[[zz]] <- NULL
    }
    nombresPolTwo <- apply(tempPOLtwo, 2, 
                           function(x){
                                  !all(is.na(x))})
    nombresPolTwo <- as.character(which(nombresPolTwo == TRUE))
    if(length(colStylePOL) > 0){
      names(colStylePOL) <- nombresPolTwo
    }
  }

  if(length(tempDIC) > 1) ncols1 <- ncol(tempDIC)  else ncols1 = 1  
  if(length(tempPOL) > 1) ncols2 <- ncol(tempPOL) else ncols2 = 1 
  if(ncols1 == 1) ncols = 2 else ncols <- max(ncols1, ncols2)

  addDataFrame(data.frame("DIF para dicotómicos"), sheet  = get(sheetOut[jj+1]),
              startRow = 8, startColumn = 1, row.names = FALSE,
              col.names = FALSE, 
              colStyle = list('1' = csEncTwo))

  addDataFrame(get(cutoffDIFF[jj])$DIC, sheet  = get(sheetOut[jj+1]),
              startRow = 10, startColumn = 1, row.names = TRUE,
              col.names = TRUE, colnamesStyle = csEncThree,
              rownamesStyle = csEncThree, 
              colStyle = list('1' = csEncFour, '2' = csEncFour)) 

  if(length(tempDIC) == 1){
    addDataFrame(tempDIC, sheet  = get(sheetOut[jj+1]),
                 startRow = 14, startColumn = 1, row.names = rownamedic,
                 col.names = colnamedic, colnamesStyle = colnamStyledic, 
                 rownamesStyle = rowStyledic) 
  
  } else {
    addDataFrame(tempDICone, sheet  = get(sheetOut[jj+1]),
              startRow = 14, startColumn = 1, row.names = rownamedic,
              col.names = colnamedic, colnamesStyle = colnamStyledic, 
              rownamesStyle = rowStyledic) 
  
    addDataFrame(tempDICtwo, sheet  = get(sheetOut[jj+1]),
              startRow = 24, startColumn = 1, row.names = rownamedic,
              col.names = FALSE, rownamesStyle = rowStyledic,
              colStyle  = colStyleTwoDIC) 
  }
  setColumnWidth(get(sheetOut[jj+1]), 1, 28)
  setColumnWidth(get(sheetOut[jj+1]), seq(ncols1) + 1, 20)  


# # An'alisis polit'omicos 
  addDataFrame(data.frame("DIF para politómicos"), sheet  = get(sheetOut[jj+1]),
              startRow = 8, startColumn = ncols + 4, row.names = FALSE,
              col.names = FALSE, 
              colStyle = list('1' = csEncTwo))

  addDataFrame(get(cutoffDIFF[jj])$POL, , sheet  = get(sheetOut[jj+1]),
              startRow = 10, startColumn = ncols + 4, row.names = TRUE,
              col.names = TRUE, colnamesStyle = csEncThree,
              rownamesStyle = csEncThree, 
              colStyle = list('1' = csEncFour, '2' = csEncFour)) 
  if(length(tempPOL) == 1) {
        addDataFrame(tempPOL, sheet  = get(sheetOut[jj+1]),
                 startRow  = 14, startColumn = ncols + 4, 
                 row.names = rownamepol,col.names = colnamepol, 
                 colnamesStyle = colnamaStylePOL, 
                 rownamesStyle = rowStylepol)       
      
    
      } else {
        addDataFrame(tempPOLone, sheet  = get(sheetOut[jj+1]),
              startRow  = 14, startColumn = ncols + 4, row.names = rownamepol,
              col.names = colnamepol, colnamesStyle = colnamaStylepol, 
              rownamesStyle = rowStylepol) 

        addDataFrame(tempPOLtwo, sheet  = get(sheetOut[jj+1]),
              startRow  = 24, startColumn = ncols + 4, row.names = rownamepol,
              col.names = FALSE, colnamesStyle = colnamaStylepol, 
              rownamesStyle = rowStylepol, colStyle = colStylePOL) 
    }
  setColumnWidth(get(sheetOut[jj+1]), ncols + 4, 28)
  setColumnWidth(get(sheetOut[jj+1]), ncols + 4 + seq(ncols2), 20)  


# # information datatable  

   addDataFrame(namesPrueba, sheet  = get(sheetOut[jj+1]),
              startRow = 1, startColumn = 1, row.names = TRUE,
              col.names = FALSE, colnamesStyle = csEncOne,
              rownamesStyle = csNeg) 

  }
  saveWorkbook(wb, file = outFile) 
#  rm(list = tempTableInvar)
}

