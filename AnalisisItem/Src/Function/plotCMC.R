# # Gráfica CMC

  plotCMC <- function(data, outPathGraph, indexData)
  {
    # # Tabla confiabilidades sin el ítem (CMC)
    indexItems <- dim(data)[2]
  if (indexItems > 3) {
    ConfTot <- CMC::alpha.curve(data[complete.cases(data),])
  } else {
  # # Se debe armar la tabla  ConfTot debido a que CMC::alpha.curve
  # # presenta error con solo 3 ítems
    maxAlphaRem <- max(indexreliability$alpha.drop$raw_alpha)
    isMaxAlphaRem <- indexreliability$alpha.drop$raw_alpha == maxAlphaRem
    itemRem <- as.character(indexreliability$alpha.drop$item[isMaxAlphaRem])

    ConfTot <- data.frame( N.Item = c(indexItems - 1,
      indexItems),
    Alpha.Max = c(maxAlphaRem, alphaCoef),
    Removed.Item = c(itemRem, '--'))
  }

  ConfTot$MaxX <- max(ConfTot[ConfTot$Alpha.Max ==
    max(ConfTot$Alpha.Max), "N.Item"])
  ConfTot$MaxY <- max(ConfTot[ConfTot$Alpha.Max ==
    max(ConfTot$Alpha.Max), "Alpha.Max"]) + 0.05
  ConfTot$MinY <- min(ConfTot[ConfTot$Alpha.Max ==
    min(ConfTot$Alpha.Max), "Alpha.Max"])


  CMCtest <- ggplot(ConfTot, aes(x = N.Item, y = Alpha.Max))
  CMCtest <- CMCtest + geom_point()
  CMCtest <- CMCtest + geom_text(aes(label = Removed.Item, angle
   = 45,
   hjust = -0.2, vjust = -0.2),
  size = 2.5)
  CMCtest <- CMCtest + geom_linerange(aes(x = MaxX, ymin = MinY,
    ymax = MaxY) )
  CMCtest <- CMCtest + scale_x_reverse("Número de ítems") + ylab("Curva de Cronbach-Mesbah")
  CMCPlotpng <- file.path(outPathGraph, paste("/CMC-", indexData, ".png", sep = ""))
  CMCPloteps <- file.path(outPathGraph, paste("/CMC-", indexData, ".eps", sep = ""))
   # # Create dir if doesn't exist
  if(!file.exists("graficos")) { #!try(dir.exists(outPathGraph))
    dir.create(file.path(outPathGraph), recursive = TRUE)
  }
  if (!file.exists(CMCPlotpng)) {
    ggsave(CMCPloteps)
    ggsave(CMCPlotpng)
  }
  }

