################################################################################
# # univariateFunctions.R
# # R Versions: 3.0.2 i386
# #
# # Author(s): Carlos Arias and Alvaro Uzaheta
# #
# # SB 359
# # Description: Functions to compute percentages by category of
# # response, apply that to a data.frame and replace NR to NA
# #
# # Inputs: NA
# #
# # Outputs: NA
# #
# # File history:
# #   20111122: Creation
# #   20140201: Update
################################################################################

RecodeToNA <- function (variable, categories)
{
  # # Recode the categories of variable to NA
  # #
  # # Arg:
  # #  variable[character|factor]: variable to recode
  # #  categories[character]: levels of variable recoding to NA
  # #
  # # Ret:
  # #  the variable after recode

    # # levels of variable
    levelsVar <- levels(variable)
    levelsVar <- levelsVar[!(levelsVar %in% categories)]

    isCat <- variable %in% categories
    if (any(isCat)) {
      variable[isCat] <- NA
# #       variable <- variable[, drop = TRUE]
    }

    variable <- ordered(variable, levels = levelsVar)
    return(variable)
}


MakeListData <- function (lista, nvar = "codItem")
{
  # # Converts from list to data.frame when is a list of data.frames
  # #
  # # Arg:
  # #  lista[list]: list of data.frame
  # #  nvar[character]: name of the variable at output with the names of
  # #  the list
  # #
  # # Ret:
  # #  a data.frame
    taml <- length(lista)
    noml <- names(lista)
    tamg <- sapply(lista, nrow)
    clasif <- NULL
    SALIDA <- NULL
    for (G in 1:taml) {
        clasif <- rep(noml[G], tamg[G])
        pega <- data.frame(clasif = clasif, lista[[G]])
        SALIDA <- rbind(SALIDA, pega)
    }
    names(SALIDA)[1] <- nvar
    return(SALIDA)
}


ComputeDistr <- function (columna, weight, cateNA, cateDrop) {
  # # Compute relative frequencies for a factor variable, the values for
  # # NR categories are reported from total distribution and valid
  # # categories are reported from distribution without omission
  # #
  # # Arg:
  # #  columna[factor]: vector to recode
  # #  weight[numeric]: vector with weights to use in distribution
  # #     estimation
  # #  categories[character]: values to recode as NA
  # #  mixed[logical]: if TRUE the mixed distribution is computed, if
  # #                  FALSE the distribution with NR is computed
  # #
  # # Ret:
  # #  data frame with categories and percetage by category

  require(car)

  levelsVar <- levels(columna)
  # # add 1 to compute absolute frecuencies
  dataAgrega <- data.frame(opRes = columna,
                           totalW = weight,
                           frecuencia = 1)

  tablaOr <- aggregate(dataAgrega[, c('frecuencia', 'totalW')],
                     list(opRes = columna), sum,
                     na.rm = TRUE)


  # # drop categories in cateDrop to doesn't report
   if (!is.null(cateDrop)) {
     isCateDrop <- tablaOr[, 'opRes'] %in% cateDrop

     tabla <- tablaOr[!isCateDrop, ]
   }


   # # compute distributions with NA and without NA
   isCate <- tabla[, 'opRes'] %in% cateNA

# #    if (mixed) {
# #      isCate <- tabla[, 'columna'] %in% categories
# #    } else {
# #      isCate <- rep(TRUE, length(tabla[, 'columna']))
# #    }

  total    <- colSums(tabla[, c('frecuencia', 'totalW')])
  totalCat <- colSums(tabla[!isCate, c('frecuencia', 'totalW')])

  # # distributions without NA
  tabla[, 'frecRelNA'] <- as.numeric(tabla[, 'frecuencia']) /
                                  as.numeric(total['frecuencia'])

  tabla[, 'frecRelWNA'] <- as.numeric(tabla[, 'totalW']) /
                                  as.numeric(total['totalW'])

  # # distributions without NA
  tabla[!isCate, 'frecRel'] <- as.numeric(tabla[!isCate, 'frecuencia']) /
                                  as.numeric(totalCat['frecuencia'])

  tabla[!isCate, 'frecRelW'] <- as.numeric(tabla[!isCate, 'totalW']) /
                                  as.numeric(totalCat['totalW'])

  # # the mixed distribution
  tabla[ isCate, 'porcentaje'] <- tabla[ isCate, 'frecRelNA']
  tabla[!isCate, 'porcentaje'] <- tabla[!isCate, 'frecRel']

  tabla[ isCate, 'porcentajePesos'] <- tabla[ isCate, 'frecRelWNA']
  tabla[!isCate, 'porcentajePesos'] <- tabla[!isCate, 'frecRelW']

  # # recode categories in cateNA in first value of the vector
  tablaDisMix <- tabla[, c(1, 8, 9)]
  if (length(cateNA) > 1) {
    recodeAs <- paste("'", cateNA[-1], "'='", cateNA[1], "'", sep = '')
    recodeAs <- paste(recodeAs, collapse = ';')

    tablaDisMix[, 'opRes'] <- Recode(tablaDisMix[, 'opRes'], recodeAs)


    tablaDisMix <- aggregate(tablaDisMix[, c('porcentaje', 'porcentajePesos')],
                     list(opRes = tablaDisMix[, 'opRes']), sum,
                     na.rm = TRUE)
  }


  # #
  tablaRet <- merge(tablaOr, tabla[, c(1, 4:7)], all = TRUE)
  tablaRet <- merge(tablaRet, tablaDisMix, all = TRUE)

  tablaRet[, 'opRes'] <- ordered(tablaRet[, 'opRes'],
                                 levels = levelsVar)

  tablaRet <- tablaRet[ordered(tablaRet[, 'opRes']), ]

  return(tablaRet)
}

MakeDistrData <- function (datBlock, cateNA, cateDrop,
                           weight = NULL, kOmisHigh = kOmisHigh,
                           kLowPer = kLowPer, kHighPer = kHighPer) {
  # # Recode some categories to NA
  # #
  # # Arg:
  # #  variable[factor]: vector to recode
  # #  categories[character]: values to recode as NA
  # #  mixed[logical]: if TRUE the mixed distribution is computed, if
  # #                  FALSE the distribution with NR is computed
  # #
  # # Ret:
  # #  vector with the variable recode
    isContinious <- sapply(datBlock, is.integer)
    formasDat <- datBlock[, !isContinious]
    formasCont <- datBlock[, isContinious]

    if (is.null(weight)) {
      weight <- rep(1, nrow(datBlock))
    }
    percentages <- lapply(formasDat, ComputeDistr, weight,
                          cateNA, cateDrop)

    resultado <- MakeListData(percentages)
    resultado[, 'marca'] <- ''

    isCatNA <- resultado[, 'opRes'] %in% cateNA
    isPorNA <- is.na(resultado[, 'porcentaje'])

    isHighOmis <- isCatNA & !isPorNA & resultado[, 'porcentaje'] >= kOmisHigh
    if (any(isHighOmis)) {
      resultado[isHighOmis, 'marca'] <- '20% o más en NR'
    }

    isLowPer <- !isCatNA & !isPorNA & resultado[, 'porcentaje'] <= kLowPer
    if (any(isLowPer)) {
      resultado[isLowPer, 'marca'] <- '5% o menos'
    }

    isHighPer <- !isCatNA & !isPorNA & resultado[, 'porcentaje'] >= kHighPer
    if (any(isHighPer)) {
      resultado[isHighPer, 'marca'] <- '95% o más'
    }

    return(resultado)
  }

# # Table for NA's verification
    
CalculateMissing  <- function(variables, datos, omissionThreshold) {
    # # Calculate some statistics of Missing information to the form
    # #
    # # Arg:
    # # variables[character]: vector with the names of the variables to
    # # compute the statistics
    # # datos[data.frame]: data.frame to compute the statistics
    # # omissionThreshold[numeric]: proportion of omission use as umbral
    # #
    # # Ret:
    # #  a vector with the statistics of omission 
    # #       variables <- names(x[, grep(indPrueba, names(x))])
      
      nTotIndiv   <- nrow(datos[, variables])
      nOmit       <- rowMeans(is.na(datos[, variables]))
      nTotCompl   <- sum(complete.cases(datos[, variables]))
      pctnComp    <- nTotCompl/nTotIndiv
      minMissi    <- min(colMeans(is.na(datos[, variables])))
      maxMissi    <- max(colMeans(is.na(datos[, variables])))
      isMiss80    <- nOmit <= omissionThreshold
      nMiss80     <- sum(isMiss80)
      pctnMiss    <- nMiss80 / nTotIndiv
      minMissi80  <- min(colMeans(is.na(datos[isMiss80, variables])))
      maxMissi80  <- max(colMeans(is.na(datos[isMiss80, variables])))


      return(data.frame(totalIndi = nTotIndiv, 
                        minMiss = minMissi, maxMiss = maxMissi, 
                        totalComp = nTotCompl, pctCompletos = pctnComp, 
                        nMiss80 = nMiss80,  pctnStuKeep = pctnMiss,   
                        minMissi80 = minMissi80, maxMissi80 = maxMissi80))
}
 
ComputeComparison <- function(data, item, frecCensal, frecControl, disCensal,
                             kHighDif, kHighDis){


 # # Calcula la distancia chi-cuadrado, la diferencia maxima absoluta y el rango de la
 # # diferencia entre la distribucion de la aplicacion censal y la control por items.
 # # data[data.frame]: data.frame with items, relative frequencies census and control
 # # item[character]: nombre de la columna en data que tiene los id de los items
 # # frecCensal[character] : nombre de la columna en data que tiene las frecuencias relativas
 # #                     de los distractores de la aplicacion censal
 # # frecCotrol[character] : nombre de la columna en data que tiene las frecuencias relativas
 # #                     de los distractores de la aplicacion control
 # # discensal[character] : nomebre de la columna en darta que tiene las frecuencias absolutas
 # #                     de los distractores de la aplicacion censal
 # #
 # # Ret:
 # #  data frame with distance, diferencia y rango por item


MakeDif      <- function(x){
                  maxDif <- max(abs(x[ ,frecCensal] - x[ ,frecControl]))
                }

MakeRange    <- function(x){
                  max(x[ ,frecCensal] - x[ ,frecControl])-
                  min(x[ ,frecCensal] - x[ ,frecControl])
                }

MakeDistanceFrec <-  function(x){
                       chisq.test(x=x[, disCensal],p=x[, frecControl])$statistic
                     }

MakeDistancePer <-  function(x){
                       chisq.test(x=x[, frecCensal],p=x[, frecControl])$statistic
                     }

  comparisons <-  list()
  difMax <- list()
  rang   <- list()
  distFrec   <- list()
  distPer   <- list()
  marl   <- list()

  items <- unique(data[, item])
  for(ll in items){
    isll = data[, item] == ll
    comparisons$difMax[[ll]] <- MakeDif(data[isll, ])
    comparisons$rang[[ll]]   <- MakeRange(data[isll, ])
    comparisons$distFrec[[ll]]   <- MakeDistanceFrec(data[isll, ])
    comparisons$distPer[[ll]]   <- MakeDistancePer(data[isll, ])
  }

  comparisons <- as.data.frame(comparisons)

  comparisons[, "codItem"]  <- items



  isOutLimitsFR <- comparisons[, 'difMax']   > kHighDif &
                   comparisons[, 'distPer']  > kHighDis



  comparisons[, 'marca'] <- ''

  if(any(isOutLimitsFR)) {
       comparisons[isOutLimitsFR, 'marca'] <- '*Dist Dif'
   }

  return(comparisons)

}


