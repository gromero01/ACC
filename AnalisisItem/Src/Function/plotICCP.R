################################################################################
# # MantelLI.R
# # R Versions: 2.15.0
# #
# # Author(s): Jorge Mario Carrasco Ortiz
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
# #   20151210: - Adapt to run ICC plots from information of Bilog
# #             - Adapt to save a plot for each item in a Index
################################################################################

irt.4PM.Info <- function(theta, item.par, D=1.7) {
   # This function calculates the Fisher information for
   # Four Parameter Model, given item parameters and ability.
   #
   # item.par should be a matrix with four columns:
   # Each row represents an item,
   # first column represents a parameters (item discrimination)
   # second column represents b parameters (item difficulty)
   # third column represents c parameters(pseudo-guessing parameter)
   # fourth column represents d parameters (upper asymptote)
   # theta can be a single number or a vector.

if (length(item.par) < 4) {
   item.par[4] = 1
}

if (length(item.par) < 3) {
   item.par[3] = 0
}

if (length(item.par) < 2) {
   item.par[2] = 1
}

return(((D * item.par[1])^2 * (item.par[4] - item.par[3]) ^ 2) /
         ((item.par[3] + item.par[4] *
           exp(D*item.par[1] * (theta - item.par[2]))) *
         (1 - item.par[3] + (1-item.par[4]) *
           exp(D*item.par[1] * (theta-item.par[2]))) *
         (1 + exp(-D * item.par[1] * (theta - item.par[2])))^2) )
}

responseCurve <- function(resBlockOri, personAbilities, methodBreaks = "Sturges",
                          dirPlot = "ICCexample.eps", plotName  = "Opciones de respuesta",
                          xlabel = "Habilidad", ylabel = "Proporción",
                          legendName = "Categorías", keyData = keyData,
                          dirCatFreq = "catFreq.Rdata", indexItems) {
  # # This function reads person ability estimates from Bilog
  # # and construct the Response Curve
  # # Arg:
  # #  resBlock:        Input data matrix or data frame with item responses
  # #  personAbilities: Data.frame with estimated habilities
  # #  methodBreaks:    A character string naming an algorithm to compute
  # #                   the number of groups
  # #  dirPlot:         The name of final plot with direction when the
  # #                   plot will save and format (.eps, .png, ...)
  # #  plotName:        The title of final plot
  # #  xlabel:          Label the x-axis of the current axes
  # #  ylabel:          Label the y-axis of the current axes
  # #  legendName:      Label the legend of the current graph
  # #  keyData:         Data frame with the key of the items (id and keyItem)
  # #  indexItems:      Variable indexItems
  # #
  # # Ret:
  # #     : Graphs in eps and png format

  # # ajustando limites cuando las habilidades estimadas se salen del
  # intervalo [-4,4]
  if( any(personAbilities$ABILITY < -4) ) {
    if( any(personAbilities$ABILITY > 4) ) {
      limXInf  <- round(min(personAbilities$ABILITY) - 1)
      limXSup  <- round(max(personAbilities$ABILITY) + 1)
      limX     <- seq(limXInf, limXSup, length.out = 100)
    } else{
      limXInf  <- round(min(personAbilities$ABILITY) - 1)
      limX     <- seq(limXInf, 4, length.out = 100)
    }
  } else {
    if ( any(personAbilities$ABILITY > 4) ) {
      limXSup  <- round(max(personAbilities$ABILITY) + 1)
      limX     <- seq(-4, limXSup, length.out = 100)
    } else{
      limX     <- seq(-4, 4, length.out = 100)
    }
  }

  # # Obtain the abilities and responses of all item
  abiliBlock <- personAbilities[, c("iSubject", "ABILITY", "SERROR")]
  abiliBlock <- merge(resBlockOri, abiliBlock, by = "iSubject")
  abiliBlock <- abiliBlock[order(abiliBlock[, ABILITY]),]

  # # Function to find the frequency and average ability
  countCategory <- function(x, colTomar, flagAbility = TRUE) {
    colTomar <- colTomar[!grepl("iSubject", colTomar)]
    if (flagAbility) {
      respFre <- rbindlist(lapply(colTomar, function(z)
                           x[, list('item' = z, 'Freq' = .N,
                                    'mAbility' = mean(ABILITY)), by = z]))
    } else {
      respFre  <- rbindlist(lapply(colTomar, function(z)
                           x[, list('item' = z, 'Freq' = .N), by = z]))
    }
    if (nrow(respFre)){
      respFre <- subset(respFre, respFre[[1]] != "")
      setnames(respFre, names(respFre)[1], "categoria")
      if (flagAbility) {
        respFre <- respFre[, list(categoria, 'prop' = Freq / sum(Freq), mAbility), by = "item"]
      } else {
        respFre <- respFre[, list(categoria, 'prop' = Freq / sum(Freq)), by = "item"]
      }
    } else {
      respFre <- NULL
    }
    return(respFre)
  }

  tablaRep <- countCategory(abiliBlock, names(resBlockOri))
  save(tablaRep, file = dirCatFreq)
  # # Break the intervals of abilities
  breaksAbili <- hist(plot = FALSE, abiliBlock[, ABILITY],
                      breaks = methodBreaks)$breaks
  cutOff  <- cut(abiliBlock[, ABILITY], breaks = breaksAbili,
                  include.lowest = TRUE)
  xempICC <- aggregate(abiliBlock[, ABILITY], by = list(cutOff),
                        FUN = mean)
  abiliBlock <- abiliBlock[, indexItems, with = FALSE]
  abiliBlock <- split(abiliBlock, f = cutOff)
  grupMal    <- names(abiliBlock)[unlist(lapply(abiliBlock, nrow)) == 0]
  for (rr in grupMal) {
    abiliBlock[[rr]] <- NULL
  }

  # # Computre probabilities for each node
  resBlock <- lapply(names(abiliBlock), function(z) {
                     auxFr <- countCategory(abiliBlock[[z]], indexItems, FALSE)
                     auxX  <- subset(xempICC, Group.1 == z)[, "x"]
                     auxFr <- subset(auxFr, !categoria %in% c("O", "M"))
                     return(cbind(auxFr, 'x' = auxX))
                   })
  resBlock <- rbindlist(resBlock)
  resBlock <- split(resBlock, f = resBlock$item)
  listGGp <- list()
  for (itName in names(resBlock)){
    # # Identify the key of each item
    optionRes <- subset(keyData, id == itName, select = "keyItem")[1, 1]
    auxTipo   <- resBlock[[itName]]$categoria == optionRes
    resBlock[[itName]] <- cbind(resBlock[[itName]], 'Tipo' = ifelse(auxTipo,
                                "Clave", "Distractor"))
    resBlock[[itName]][categoria == optionRes, categoria := paste0(categoria, "*")]


    finalPlot <- ggplot(resBlock[[itName]], aes(x = x, y = prop, colour = categoria, linetype = Tipo)) +
                   geom_line(size = 0.6)  +
                   labs(x = xlabel, y = ylabel, title = paste0(plotName," - ", itName)) +
                   geom_point(data = resBlock[[itName]], mapping = aes(x = x, y = prop,
                              colour = categoria), shape = 18, size = 2.5) +
                   scale_colour_discrete(name = legendName) + theme_bw() +
                   theme(legend.position = c(1, 1), legend.justification = c(1, 1))
    dirAux <- sapply(dirPlot, function(x) gsub("(_V.+\\.png)",
                         paste0("_", itName, "\\1"), x))
    listGGp[[itName]] <- list('graph' = finalPlot, 'dir' = dirAux)
    sapply(dirAux, function(x) ggsave(x, width = 10))
  }
  return(listGGp)
}

plotICCB <- function (itemParameters, resBlock, personAbilities, 
                      scaleD    = 1.702, methodBreaks = "Sturges",
                      dirPlot   = "ICCexample.eps", namesubCon = NULL,
                      plotName  = "Curvas ICC", xlabel = "Habilidad",
                      ylabel    = "Probabilidad", legendName = "Categorías",
                      flagGrSep = FALSE, alpha = 0.05, prueba = NULL, 
                      dirSalida = outPath, indexItems, codModel) {

  # # This function reads person ability estimates from Bilog
  # # and construct the ICC graphs
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
  # #  alpha:           confidence level to ICC interval
  # #  indexItems:      variable indexItems
  # #
  # # Ret:
  # #     : Graphs in eps and png format
  # threshold parameter
  if (is.null(prueba)) {
    stop("ERROR::: prueba en la funcion plotICCB no debe ser NULL")
  }
   
    dkItems <- data.frame('step.1' = itemParameters[, "dif"])
    names(dkItems) <- gsub(" ", ".", names(dkItems))
    dkItems <- reshape(dkItems, varying = colnames(dkItems),
                       direction = "long")
    dkItems <- dkItems[order(dkItems[, "id"]), ]

    # guessing
    cjItems <- itemParameters[, "azar"]


    # discrimination
    ajItems <- itemParameters[, "disc"]
    numRept <- rep(max(dkItems[, "time"]), length(ajItems))
    ajItems <- rep(ajItems, numRept)
    nameIt  <- rep(indexItems, numRept)

    paramICC <- data.frame('itemName' = nameIt, 'category' = dkItems[,
                           "time"] + 1, 'Location' = dkItems[, "step"],
                           'discrimination' = ajItems, 'azar' = cjItems)
    paramICC <- split(paramICC, f = paramICC[, "itemName"])

    if (any(sapply(paramICC, nrow) > 1)) {
      stop("Revisar codigos existen items duplicados")
    }

    # # ajustando limites cuando las habilidades estimadas se salen del
    # intervalo [-4,4]
    if( any(personAbilities$ABILITY < -4) ) {
      if( any(personAbilities$ABILITY > 4) ) {
        limXInf  <- round(min(personAbilities$ABILITY) - 1)
        limXSup  <- round(max(personAbilities$ABILITY) + 1)
        limX     <- seq(limXInf, limXSup, length.out = 100)
      } else{
        limXInf  <- round(min(personAbilities$ABILITY) - 1)
        limX     <- seq(limXInf, 4, length.out = 100)
      }
    } else {
      if ( any(personAbilities$ABILITY > 4) ) {
        limXSup  <- round(max(personAbilities$ABILITY) + 1)
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
      auxItemICC <- itemICC
      itemICC   <- lapply(itemICC, function(z) {
                          cj  <- as.numeric(as.character(z["azar"]))
                          bjk <- as.numeric(as.character(z["Location"]))
                          aj  <- as.numeric(as.character(z["discrimination"]))
                          return(cj + (1 - cj) * plogis(limX, location = bjk,
                                        scale = 1/(scaleD * aj)))
                        })
      plotINFO <- lapply(auxItemICC, function(z) { 
                          cj  <- as.numeric(as.character(z["azar"]))
                          bjk <- as.numeric(as.character(z["Location"]))
                          aj  <- as.numeric(as.character(z["discrimination"]))
                          irt.4PM.Info(limX, c(aj, bjk, cj))})      
      x         <- rep(limX, length(nameCategory))
      categoria <- rep(nameCategory, sapply(itemICC, length))
      y         <- unlist(itemICC)
      item      <- rep(zz, length(categoria))
      itemICC   <- data.frame(x, y, item, categoria, curva = "ICC")
      plotINFO  <- data.frame(x, 'y' = unlist(plotINFO), item, 
                              categoria, curva = "INFO") 
      curves    <- rbind(curves, itemICC, plotINFO)
    }

    curves[, 'categoria'] <- factor(curves[, 'categoria'])

    ntheDiff <- length(unique(personAbilities[, "ABILITY"]))
    if (is.null(namesubCon)){
      namesubCon <- ""
    }    
    ntheDiff <- data.frame('Numero_Diferentes' = ntheDiff,
                           'Indice' = namesubCon, 'Prueba' = prueba)

    fileThetaDif <- file.path(dirSalida, "corridas/thetasDiferentes.txt")
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
    abiliBlock <- personAbilities[, c("iSubject", "ABILITY", "SERROR")]
    abiliBlock <- merge(resBlock, abiliBlock, by = "iSubject")
    abiliBlock <- abiliBlock[order(abiliBlock[, ABILITY]),]
    #cateName   <- sort(unique(unlist(lapply(abiliBlock[, indexItems, with = FALSE], unique))))
    #cateName   <- cateName[-1]

    # # Building Empiric ICC

    # breaksAbili <- seq(min(abiliBlock[, "ABILITY"]),
    #                    max(abiliBlock[, "ABILITY"]), length.out = 9)

    breaksAbili <- hist(plot = FALSE, abiliBlock[, ABILITY],
                        breaks = methodBreaks)$breaks
    cutOff  <- cut(abiliBlock[, ABILITY], breaks = breaksAbili,
                   include.lowest = TRUE)
    xempICC <- aggregate(abiliBlock[, ABILITY], by = list(cutOff),
                         FUN = mean)
    abiliBlock <- split(abiliBlock, f = cutOff)

    grupMal    <- names(abiliBlock)[unlist(lapply(abiliBlock, nrow)) == 0]
    for (rr in grupMal) {
      abiliBlock[[rr]] <- NULL
    }

  # # FALTA definir si se incluyen los de multimarca y omision

    abiliBlock <- lapply(names(abiliBlock), function(z){
                       acIt <- colSums(abiliBlock[[z]] == 1, na.rm = TRUE)
                       nIt  <- colSums(!is.na(abiliBlock[[z]]))
                       pL   <- qbeta(alpha / 2, acIt, nIt - acIt + 1)
                       pU   <- qbeta(1 - (alpha / 2), acIt + 1, nIt - acIt)
                       prob <- acIt / nIt
                       prob <- replace(acIt / nIt, is.nan(prob), 0)
                       auxX <- subset(xempICC, Group.1 == z)[, "x"]
                       data.table('y' = prob, 'categoria' = 1,
                                  'item' = names(prob),
                                  'x' = auxX, pL, pU)
                      })

    empiricICC <- rbindlist(abiliBlock)
    rm(abiliBlock, resBlock)
    empiricICC[, categoria := as.factor(empiricICC$categoria)]
    listGGp    <- list()
    if (!flagGrSep) {
      finalPlot <- ggplot(curves, aes(x = x, y = y)) + geom_line() +
                   facet_wrap(~item) + labs(x = xlabel, y = ylabel,
                                            title = plotName) +
                   geom_point(data = empiricICC,
                              mapping = aes(x = x, y = y)) +
                   geom_ribbon(data = empiricICC,
                              aes(ymin = pL,ymax = pU),alpha=0.3) +
                   theme_bw()
      listGGp[[itName]] <- list('graph' = finalPlot, 'dir' = dirPlot)
      sapply(dirPlot, function(x) ggsave(x, width = 10))
    } else {
      itemCurve  <- split(curves, f = curves$item)
      empiricICC <- split(empiricICC, f = empiricICC$item)
      for (itName in names(itemCurve)){
        auxAlp <- round(paramICC[[itName]]["discrimination"], 3)
        auxBet <- round(paramICC[[itName]]["Location"], 3)
        auxAza <- round(paramICC[[itName]]["azar"], 3)
        maxINFO <- max(subset(itemCurve[[itName]], curva == "INFO")$y)
        facScal <- ifelse(1 / maxINFO < 4, 1 / maxINFO, 4)
        maxINFO <- ifelse(maxINFO < 1, maxINFO  * facScal, maxINFO)
        textSize <- 20
        # # Grafico 
        infoPlt   <- ggplot(subset(itemCurve[[itName]], curva == "INFO"), 
                            aes(x = x, y = y)) + theme_bw(18) + expand_limits( y = c(0, maxINFO)) + 
                     geom_line(size = 0.3, linetype = "dashed", colour = "red") + labs(y = "")        
        finalPlot <- ggplot(subset(itemCurve[[itName]], curva == "ICC"), 
                            aes(x = x, y = y)) + 
                     geom_line(aes(linetype = "ICC", colour="ICC"), size = 0.8) + 
                     labs(x = xlabel, y = ylabel) + facet_wrap(~item) +
                     geom_point(data = empiricICC[[itName]], mapping = aes(x = x, y = y), shape = textSize) +
                     geom_line(data = empiricICC[[itName]], mapping = aes(x = x, y = y, linetype = "Information", colour="Information"), alpha = 0.5, size = 1) + 
                     geom_line(data = empiricICC[[itName]], mapping = aes(x = x, y = y, linetype = "Empiric", colour="Empiric"), alpha = 0.5, size = 1) +
                     scale_colour_manual(name="", values= c("blue", "black", "red")) + 
                     scale_linetype_manual(name="", values=c("dashed", "solid", "dashed")) + 
                     geom_ribbon(data = empiricICC[[itName]], aes(ymin = pL,ymax = pU),
                                 linetype = 2, fill = "blue", colour = "blue", alpha = 0.25) +
                     #annotate("rect", xmin = -4.3, xmax = -3, ymin = 0.875, ymax = 1, alpha = .1) +
                     annotate("text", x = -3, y = 0.965, label = paste("alpha (Discriminicación) == ", auxAlp), color = "black", parse = TRUE, size = 6) +
                     annotate("text", x = -3, y = 0.915, label = paste("beta (Dificultadad) == ", auxBet), color = "black", parse = TRUE, size = 6) +
                     theme(legend.title = element_text(), # switch off the legend title
                          legend.key.size = unit(1.5, "lines"),
                          legend.key = element_rect(fill = "white"))# switch off the rectangle around symbols in the legend)
        if (codModel == "07") {
          finalPlot <- finalPlot + annotate("text", x = -3, y = 0.865, label = paste("c (Azar)== ", auxAza), 
                                            color = "black", parse = TRUE, size = 6)
        }                    
        finalPlot <- finalPlot  + theme_bw(textSize) +  theme(legend.position = "bottom")
        arrangeFinal <- ggplot_dual_axis(finalPlot, infoPlt)
        dirAux <- sapply(dirPlot, function(x) gsub("(_V.+\\.png)",
                         paste0("_", itName, "\\1"), x))
        listGGp[[itName]] <- list('graph' = arrangeFinal, 'dir' = dirAux)
        sapply(dirAux, function(x) ggsave(file = x, plot = arrangeFinal, width = 410, height = 297, units = "mm"))
      }
    }
    return(listGGp)
}

plotICCP <- function (itemParameters, resBlock, personAbilities,
                      scaleD    = 1.702, methodBreaks = "Sturges",
                      dirPlot   = "ICCexample.eps", namesubCon = NULL,
                      plotName  = "Curvas ICC", xlabel = "Habilidad",
                      ylabel    = "Probabilidad", legendName = "Categorías",
                      flagGrSep = FALSE, prueba = NULL, dirSalida = outPath) {

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

  if (is.null(prueba)) {
    stop("ERROR::: prueba en la funcion plotICCB no debe ser NULL")
  }
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
                           'Indice' = namesubCon, 'Prueba' = prueba)

    fileThetaDif <- file.path(dirSalida, "corridas/thetasDiferentes.txt")
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
    cateName   <- sort(unique(unlist(lapply(abiliBlock, unique))))
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
    abiliBlock <- split(abiliBlock, f = cutOff)

    grupMal    <- names(abiliBlock)[unlist(lapply(abiliBlock, nrow)) == 0]
    for (rr in grupMal) {
      abiliBlock[[rr]] <- NULL
    }
    countNA <- lapply(abiliBlock, function(x)
                      x[, lapply(.SD, function(z) mean(is.na(z)) == 1)])
    countNA <- unlist(lapply(countNA, function(x) names(x)[x]))

    fileProInt <- file.path(dirSalida, "corridas/problemasICC.txt")

    if (length(countNA) != 0){
      countNA <- data.frame('Intervalo' = names(countNA),
                            'Item_All_NA' = as.character(countNA),
                            'Indice' = namesubCon, 'Prueba' = prueba)

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
    rm(abiliBlock, resBlock)
    empiricICC[, "categoria"] <- as.factor(empiricICC[, "categoria"])

    if (!flagGrSep) {
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
    } else {
      itemCurve <- split(curves, f = curves$item)
      empiricICC <- split(empiricICC, f = empiricICC$item)
      for (itName in names(itemCurve)){
        if (kCategories != 2) {
          finalPlot <- ggplot(itemCurve[[itName]], aes(x = x, y = y, colour = categoria)) +
                       geom_line(aes(colour = categoria))  +
                       labs(x = xlabel, y = ylabel, title = plotName) +
                       geom_point(data = empiricICC[[itName]], mapping = aes(x = x, y = y,
                                  colour = categoria)) +
                       scale_colour_discrete(name = legendName) + theme_bw()
        } else {
          finalPlot <- ggplot(itemCurve[[itName]], aes(x = x, y = y)) + geom_line() +
                       labs(x = xlabel, y = ylabel, title = plotName) +
                       geom_point(data = empiricICC[[itName]], mapping = aes(x = x, y = y)) +
                       theme_bw()
        }
        dirAux <- sapply(dirPlot, function(x) gsub("(_V.+\\.png)", paste0("_", itName, "\\1"), x))
        sapply(dirAux, function(x) ggsave(x, width = 10))
      }
    }

}


plotICCW <- function (itemParameters, resBlock, personAbilities,
                      scaleD = 1, methodBreaks = "Sturges",
                      dirPlot = "ICCexample.eps", namesubCon = NULL,
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
    if (is.null(namesubCon)){
      namesubCon <- ""
    }
    ntheDiff <- data.frame('Numero_Diferentes' = ntheDiff,
                           'Indice' = namesubCon, 'Prueba' = prueba)

    fileThetaDif <- file.path(dirSalida, "corridas/thetasDiferentes.txt")
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
    cateName   <- sort(unique(unlist(lapply(abiliBlock, unique))))
    cateName   <- cateName[-1]

    # # Building Empiric ICC

    breaksAbili <- hist(plot = FALSE, abiliBlock[, "ability"],
                        breaks = 20)$breaks

    cutOff  <- cut(abiliBlock[, "ability"], breaks = breaksAbili,
                   include.lowest = TRUE)
    xempICC <- aggregate(abiliBlock[, "ability"], by = list(cutOff),
                         FUN = mean)
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
                            'Indice' = namesubCon, 'Prueba' = prueba)

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

################################################################################
# # Calculando Pendientes de grafica opciones de respuesta
################################################################################

