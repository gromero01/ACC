################################################################################
# # MantelLI.R
# # R Versions: 2.15.0
# #
# # Author(s): Jorge Marop Carrasco Ortiz
# #
# # Description: Displays the empiric ICC vs theorical ICC for all
# #
# # Inputs: None
# #
# # Outputs: Function
# #
# # File history:
# #   20130312: Creation by Jorge Carrasco for structure in FA scales
# #   20140525: Changing boundaries of the graphic when skills do not
# #             fall between -4 and 4  (Sandra Ropero)
################################################################################

plotICCP <- function (itemParameters, resBlock, personAbilities,
                      scaleD = 1.702, methodBreaks = "Sturges",
                      dirPlot = "ICCexample.eps", nameIndice = NULL,
                      plotName = "Curvas ICC", xlabel = "Habilidad",
                      ylabel = "Probabilidad", legendName = "Categorías") {

  # # This function reads person ability estimates from Parscale
  # #
  # # Arg:
  # #  itemParameters:  Item parameters in the format used by
  # #                   the simulation function
  # #  resBlock:        Input data matrix or data frame with item responses
  # #  personAbilities: Data.frame with estimated habilities
  # #  scaleD:          Constant of logistic model
  # #  methodBreaks:    A character string naming an algorithm to compute
  # #                   the number of groups
  # #  dirPlot:         The name of final plot with direction when the
  # #                   plot will save and format (.eps, .png, ...)
  # #  plotName:        The title of final plot
  # #  xlabel:          Label the x-axis of the current axes
  # #  ylabel:          Label the y-axis of the current axes
  # #  legendName:      Label the legend of the current graph
  # #
  # # Ret:
  # #     : Graphs in eps and png format
  # threshold parameter

    kCategories <- length(grep("step", names(itemParameters))) + 1
    dkItems     <- itemParameters[, "location"] -
                   itemParameters[, grep("step", names(itemParameters))]
    if (kCategories == 2) {
      dkItems <- data.frame('step.1' = dkItems)
    }

    names(dkItems) <- gsub(" ", ".", names(dkItems))
    dkItems <- reshape(dkItems, varying = colnames(dkItems),
                       direction = "long")
    dkItems <- dkItems[order(dkItems[, "id"]), ]

  # discrimination
    ajItems <- itemParameters[, "discrimination"]
    numRept <- rep(max(dkItems[, "time"]), length(ajItems))
    ajItems <- rep(ajItems, numRept)
    nameIt  <- rep(itemParameters[, "itemName"], numRept)

    paramICC <- data.frame('itemName' = nameIt, 'category' = dkItems[,
                           "time"] + 1, 'Location' = dkItems[, "step"],
                           'discrimination' = ajItems)
    paramICC <- split(paramICC, f = paramICC[, "itemName"])

    # # ajustando limites cuando las habilidades estimadas se salen del
    # intervalo [-4,4]
    if( any(personAbilities$ability < -4) ) {
      if( any(personAbilities$ability > 4) ) {
        limXInf  <- round(min(personAbilities$ability) - 1)
        limXSup  <- round(max(personAbilities$ability) + 1)
        limX     <- seq(limXInf, limXSup, length.out = 100)
      } else{
        limXInf  <- round(min(personAbilities$ability) - 1)
        limX     <- seq(limXInf, 4, length.out = 100)
      }
    } else {
      if ( any(personAbilities$ability > 4) ) {
        limXSup  <- round(max(personAbilities$ability) + 1)
        limX     <- seq(-4, limXSup, length.out = 100)
      } else{
        limX     <- seq(-4, 4, length.out = 100)
      }
    }

    curves   <- NULL

    # Curves theoretical ICC

    for (zz  in names(paramICC)) {
      nameCategory       <- paramICC[[zz]][, "category"]
      itemICC            <- as.data.frame(t(paramICC[[zz]]))
      colnames(itemICC)  <- nameCategory
      itemICC <- lapply(itemICC, function(z) {
                          bjk <- as.numeric(as.character(z["Location"]))
                          aj  <- as.numeric(as.character(z["discrimination"]))
                          return(plogis(limX, location = bjk,
                                        scale = 1/(scaleD * aj)))
                        })
      x            <- rep(limX, length(nameCategory))
      categoria    <- rep(nameCategory, sapply(itemICC, length))
      y            <- unlist(itemICC)
      item         <- rep(zz, length(categoria))
      itemICC <- data.frame(x, y, item, categoria)
      curves  <- rbind(curves, itemICC)
    }

    curves[, 'categoria'] <- factor(curves[, 'categoria'])

    ntheDiff <- length(unique(personAbilities[, "ability"]))
    ntheDiff <- data.frame('Numero_Diferentes' = ntheDiff,
                           'Indice' = nameIndice, 'Prueba' = kk)

    fileThetaDif <- file.path(outPath, "corridas/thetasDiferentes.txt")
    if (file.exists(fileThetaDif)) {
      ntheAux  <- read.table(file = fileThetaDif, header =
                             TRUE, sep = " ")
      ntheAux  <- subset(ntheAux,
                         !ntheAux[, "Indice"] %in% ntheDiff[, "Indice"])
      ntheDiff <- rbind(ntheAux,  ntheDiff)
    }

    write.table(ntheDiff, file = fileThetaDif, append = FALSE, row.names =
                FALSE)

    # # Obtain the abilities and responses of all item

    abiliBlock <- personAbilities[, c("iSubject", "ability", "seAbility")]

    abiliBlock <- merge(resBlock, abiliBlock, by = "iSubject")
    abiliBlock <- abiliBlock[order(abiliBlock[, "ability"]),]
    maxCate    <- sapply(abiliBlock[, indexItems], max,  na.rm = TRUE)
    cateName   <- sort(unique(unlist(lapply(abiliBlock[, indexItems], unique))))
    cateName   <- cateName[-1]
    
    # # Building Empiric ICC

    # breaksAbili <- seq(min(abiliBlock[, "ability"]),
    #                    max(abiliBlock[, "ability"]), length.out = 9)

    breaksAbili <- hist(plot = FALSE, abiliBlock[, "ability"],
                        breaks = methodBreaks)$breaks

    cutOff  <- cut(abiliBlock[, "ability"], breaks = breaksAbili,
                   include.lowest = TRUE)
    xempICC <- aggregate(abiliBlock[, "ability"], by = list(cutOff),
                         FUN = mean)

    abiliBlock <- abiliBlock[, indexItems]
    abiliBlock <- split(abiliBlock, f = cutOff)

    grupMal    <- names(abiliBlock)[unlist(lapply(abiliBlock, nrow)) == 0]
    for (rr in grupMal) {
      abiliBlock[[rr]] <- NULL
    }

    countNA <- lapply(abiliBlock, function(x) colMeans(is.na(x)) == 1)
    countNA <- unlist(lapply(countNA, function(x) names(x)[x]))

    fileProInt <- file.path(outPath, "corridas/problemasICC.txt")

    if (length(countNA) != 0){
      countNA <- data.frame('Intervalo' = names(countNA),
                            'Item_All_NA' = as.character(countNA),
                            'Indice' = nameIndice, 'Prueba' = kk)

      if (file.exists(fileProInt)) {
        counAux  <- read.table(file = fileProInt, header = TRUE, sep = " ")
        counAux  <- subset(counAux,
                           !counAux[, "Indice"] %in% countNA[, "Indice"])
        countNA  <- rbind(counAux, countNA)
      }

      write.table(countNA, file = fileProInt, append = FALSE, row.names =
                  FALSE)
    } else {
      if (file.exists(fileProInt)) {
        file.remove(fileProInt)
      }
   }

    abiliBlock <- lapply(abiliBlock, function(z){                       
                       prob <- unlist(lapply(cateName,
                                             function(x){
                                               colMeans(z >= x, na.rm = TRUE)
                                             }))
                       prob <- replace(prob, is.nan(prob), 0)
                       cate <- rep(cateName, rep(ncol(z), length(cateName)))
                       data.frame('y' = prob, 'categoria' = cate,
                                  'item' = names(prob))
                      })

    empiricICC <- NULL
    for (pp in names(abiliBlock)) {
      abiliBlock[[pp]] <- data.frame(abiliBlock[[pp]], 'x' =
                                     subset(xempICC, Group.1 == pp)[, "x"])
      empiricICC <- rbind(empiricICC, abiliBlock[[pp]])
    }
    # # Quitando calculo de categorias locas
    maxCate    <- data.frame('item' = as.character(names(maxCate)), maxCate)
    empiricICC[, "item"] <- as.character(empiricICC[, "item"])
    empiricICC <- merge(empiricICC, maxCate, by = "item")
    empiricICC <- subset(empiricICC, categoria <= maxCate)
    empiricICC[, "maxCate"] <- NULL

    rm(abiliBlock, resBlock)
    empiricICC[, "categoria"] <- as.factor(empiricICC[, "categoria"])


    if (kCategories != 2) {
      finalPlot <- ggplot(curves, aes(x = x, y = y, colour = categoria)) +
                   geom_line(aes(colour = categoria))  +
                   facet_wrap(~item) + labs(x = xlabel, y = ylabel,
                                            title = plotName) +
                   geom_point(data = empiricICC,
                              mapping = aes(x = x, y = y,
                                            colour = categoria)) +
                   scale_colour_discrete(name = legendName) + theme_bw()
    } else {
      finalPlot <- ggplot(curves, aes(x = x, y = y)) + geom_line() +
                   facet_wrap(~item) + labs(x = xlabel, y = ylabel,
                                            title = plotName) +
                   geom_point(data = empiricICC,
                              mapping = aes(x = x, y = y)) +
                   theme_bw()
    }

    sapply(dirPlot, function(x) ggsave(x, width = 10))
}


plotICCW <- function (itemParameters, resBlock, personAbilities,
                      scaleD = 1, methodBreaks = "Sturges",
                      dirPlot = "ICCexample.eps", nameIndice = NULL,
                      plotName = "Curvas ICC", xlabel = "Habilidad",
                      ylabel = "Probabilidad") {

  # # This function reads person ability estimates from Parscale
  # #
  # # Arg:
  # #  itemParameters:  Item parameters in the format used by
  # #                   the simulation function
  # #  resBlock:        Input data matrix or data frame with item responses
  # #  personAbilities: Data.frame with estimated habilities
  # #  scaleD:          Constant of logistic model
  # #  methodBreaks:    A character string naming an algorithm to compute
  # #                   the number of groups
  # #  dirPlot:         The name of final plot with direction when the
  # #                   plot will save and format (.eps, .png, ...)
  # #  plotName:        The title of final plot
  # #  xlabel:          Label the x-axis of the current axes
  # #  ylabel:          Label the y-axis of the current axes
  # #  legendName:      Label the legend of the current graph
  # #
  # # Ret:
  # #     : Graphs in eps and png format
  # threshold parameter

    dkItems <- itemParameters[, "difficulty"]
    paramICC <- data.frame('itemName' = itemParameters[, "item"],
                           'Location' = dkItems,
                           'discrimination' = 1)
    paramICC <- split(paramICC, f = paramICC[, "itemName"])
    limX     <- seq(-4, 4, length.out = 100)
    curves   <- NULL

    # Curves theoretical ICC

    for (zz  in names(paramICC)) {
      bj   <- paramICC[[zz]][, "Location"]
      aj   <- paramICC[[zz]][, "discrimination"]
      y    <- plogis(limX, location = bj, scale = 1/(scaleD * aj))
      itemICC <- data.frame('x' = limX, y, 'item' = zz)
      curves  <- rbind(curves, itemICC)
    }

    ntheDiff <- length(unique(personAbilities[, "ability"]))
    ntheDiff <- data.frame('Numero_Diferentes' = ntheDiff,
                           'Indice' = nameIndice, 'Prueba' = kk)

    fileThetaDif <- file.path(outPath, "corridas/thetasDiferentes.txt")
    if (file.exists(fileThetaDif)) {
      ntheAux  <- read.table(file = fileThetaDif, header =
                             TRUE, sep = " ")
      ntheAux  <- subset(ntheAux,
                         !ntheAux[, "Indice"] %in% ntheDiff[, "Indice"])
      ntheDiff <- rbind(ntheAux,  ntheDiff)
    }

    write.table(ntheDiff, file = fileThetaDif, append = FALSE, row.names =
                FALSE)

    # # Obtain the abilities and responses of all item

    abiliBlock <- personAbilities[, c("personId", "ability", "se")]

    abiliBlock <- merge(resBlock, abiliBlock, by = "personId")
    abiliBlock <- abiliBlock[order(abiliBlock[, "ability"]),]
    cateName   <- sort(unique(unlist(lapply(z, unique))))
    cateName   <- cateName[-1]

    # # Building Empiric ICC

    breaksAbili <- hist(plot = FALSE, abiliBlock[, "ability"],
                        breaks = 20)$breaks

    cutOff  <- cut(abiliBlock[, "ability"], breaks = breaksAbili,
                   include.lowest = TRUE)
    xempICC <- aggregate(abiliBlock[, "ability"], by = list(cutOff),
                         FUN = mean)

    abiliBlock <- abiliBlock[, indexItems]
    abiliBlock <- split(abiliBlock, f = cutOff)

    grupMal    <- names(abiliBlock)[unlist(lapply(abiliBlock, nrow)) == 0]
    for (rr in grupMal) {
      abiliBlock[[rr]] <- NULL
    }

    countNA <- lapply(abiliBlock, function(x) colMeans(is.na(x)) == 1)
    countNA <- unlist(lapply(countNA, function(x) names(x)[x]))

    fileProInt <- file.path(outPath, "corridas/problemasICC.txt")

    if (length(countNA) != 0){
      countNA <- data.frame('Intervalo' = names(countNA),
                            'Item_All_NA' = as.character(countNA),
                            'Indice' = nameIndice, 'Prueba' = kk)

      if (file.exists(fileProInt)) {
        counAux  <- read.table(file = fileProInt, header = TRUE, sep = " ")
        counAux  <- subset(counAux,
                           !counAux[, "Indice"] %in% countNA[, "Indice"])
        countNA  <- rbind(counAux, countNA)
      }

      write.table(countNA, file = fileProInt, append = FALSE, row.names =
                  FALSE)
    } else {
      if (file.exists(fileProInt)) {
        file.remove(fileProInt)
      }
   }

   abiliBlock <- lapply(abiliBlock, function(z){
                       prob <- unlist(lapply(cateName,
                                             function(x){
                                               colMeans(z >= x, na.rm = TRUE)
                                             }))
                       prob <- replace(prob, is.nan(prob), 0)
                       data.frame('y' = prob, 'item' = names(prob))
                      })

    empiricICC <- NULL
    for (pp in names(abiliBlock)) {
      abiliBlock[[pp]] <- data.frame(abiliBlock[[pp]], 'x' =
                                     subset(xempICC, Group.1 == pp)[, "x"])

      empiricICC <- rbind(empiricICC, abiliBlock[[pp]])
    }
    rm(abiliBlock, resBlock)

    finalPlot <- ggplot(curves, aes(x = x, y = y)) +
                 geom_line()  +  facet_wrap(~item) +
                 labs(x = xlabel, y = ylabel, title = plotName) +
                 geom_point(data = empiricICC, mapping = aes(x = x, y = y)) +
                 theme_bw()

    sapply(dirPlot, function(x) ggsave(x, width = 10))
}



