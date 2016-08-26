################################################################################
# # univariateFunctions.R
# # R Versions: 3.2.3 x64
# #
# # Author(s): Jorge Mario Carrasco and William Acero
# #
# # SB ALL
# # Description: Function to compute polychoric correlation of items and
# #              make parallel analysis
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
# # Function to make parallel analysis
##############################################################

MakeExploratory <- function(kFactors, rotation, dictVarPrueba, corExpBlock){
  Items <- dictVarPrueba[, 'id']
  blockPCA   <- princomp(covmat = corExpBlock$cor)
  
  if(kFactors == 1){
    rotation = ''
  }

  # # Factorial loadings
  blockEFAP  <- paste(rotation,"(blockPCA$loadings[, 1:kFactors])", sep='')
  blockEFA   <- eval(parse(text = blockEFAP))
  if(kFactors == 1){
    cargasFA   <- blockEFA
  } else{
    cargasFA   <- unclass(blockEFA$loadings)
  }
  cargasFAdf <- data.frame(Item = Items, cargasFA = cargasFA)
  colnames(cargasFAdf) <- c('Item', paste("Factor", 1:kFactors))

  # # Correlation
  if(kFactors == 1){
      compCorrelation <- as.matrix(1)
  } else {
    if (rotation == "oblimin"){
      compCorrelation <- blockEFA$Phi
    } 
    if (rotation == "Varimax"){
      compCorrelation <- diag(kFactors)
    }
  }

  rownames(compCorrelation) <- paste("Comp.", 1:kFactors, sep = '')
  colnames(compCorrelation) <- paste("Comp.", 1:kFactors, sep = '')

  # # Communalities
  if(kFactors == 1){
      communalities <- as.vector(cargasFA^2)
  } else{
      common          <- blockEFA$loadings %*% t(blockEFA$loadings)
      communalities   <- diag(common)
  }
  communalitiesdf <- data.frame(Item = Items, comunalidades = communalities)
  colnames(communalitiesdf) <- c('Item', 'Extraction')

  # # Variance Explained 
  component <- kFactors
  blockDesv <- blockPCA$sdev^2
  blockVar  <- blockDesv[kFactors]
  var       <- (blockDesv / sum(blockDesv))[kFactors]
  cumVar    <- (cumsum(blockDesv)/sum(blockDesv))[kFactors]
  expVar    <- data.frame(component, cumVar)
  colnames(expVar) <- c('Component', '% Retained Variance') 
  parList <- list(Loadings = cargasFAdf,
                  Communalities = communalitiesdf,
                  Correlation = compCorrelation,
                  VarExplained = expVar)   
  return(parList)
}

##############################################################
# # Function to make Confirmatory and Exploratory analysis
##############################################################

MakeCorrelation <- function(kkBlock, outPathSamPba, verDataIn, 
                            auxPru, semilla, tamMue, flagExplo = TRUE,
                            varId, useCor){  
   require(polycor)
   # # Obtain sample
    confData <- file.path(outPathSamPba,
                         paste("confirmatorySample_", auxPru, "_V",
                         verDataIn, ".RData", sep = ""))
     expData  <- file.path(outPathSamPba,
                           paste("exploratorytorySample_", auxPru, "_V",
                           verDataIn, ".RData", sep = ""))

     if (!file.exists(confData) & !file.exists(expData)) {  
      # # Select Exploratory

      set.seed(as.numeric(semilla))     
      isExploratory <- sample(x = rownames(kkBlock), size = ceiling(nrow(kkBlock) * tamMue))      
      expkkBlock    <- kkBlock[isExploratory, varId]
      rownames(expkkBlock) <- kkBlock[isExploratory, "SNP"]
      nObsExploratory <- nrow(expkkBlock)
      expkkBlock <- structure(expkkBlock)

       # # Select Confirmatory
      if(tamMue == 1){
        warning("Esta utilizando el 100% en exploratio y Confirmatorio")
        isConfirmatory <- isExploratory
      } else {
        isConfirmatory <- -as.numeric(isExploratory)
      }    
      confkkBlock           <- kkBlock[isConfirmatory, varId]
      rownames(confkkBlock) <- kkBlock[isConfirmatory, "SNP"]
      nObsConfirmatory      <- nrow(confkkBlock)
      save(expkkBlock, nObsExploratory, file = expData)
      save(confkkBlock, nObsConfirmatory, file = confData)
    } else {
      if (flagExplo) load(expData)
      if (!flagExplo) load(confData)
    }

     # # Obtain correlation matrix
     corExpData   <- file.path(outPathSamPba,
                               paste("corExploratory_", auxPru, "_V",
                               verDataIn, ".RData", sep = ""))
     corConfData  <- file.path(outPathSamPba,
                               paste("corConfirmatory_", auxPru, "_V",
                               verDataIn, ".RData", sep = ""))

     if (!file.exists(corExpData) & !file.exists(corConfData)) {
       corExpBlock  <- hetcor(expkkBlock, pd = TRUE, use = useCor, std.err =
                            FALSE, ML = FALSE)
       
       corConfBlock <- hetcor(confkkBlock, pd = TRUE, use = useCor, std.err =
                            FALSE, ML = FALSE)
       if(class(corConfBlock) != "try-error" & class(corExpBlock) != "try-error"){
          save(corExpBlock,  file = corExpData)
          save(corConfBlock, file = corConfData)
          cat("Listo analisis Exploratorio y Confirmatorio de :", auxPru , "\n")
        } else {
          stop(cat("No se estimo la matriz policórica inicial\n\n"))
        }
    } else {
      if (flagExplo) load(corExpData)
      if (!flagExplo) load(corConfData)
    }
    if (flagExplo) return(list(corBlock = corExpBlock, sampleBlock = expkkBlock))
    if (!flagExplo) return(list(corBlock = corConfBlock, sampleBlock = confkkBlock))
}