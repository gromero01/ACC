######################################################################
# # Funcion que calcula la varianza para censal
# # By   : María Ferndanda Zárate
# # Data : 27/01/2015
######################################################################


library(plyr)
library(dummies)
CalculaErrorCensal <- function(data, agrega, y, dominio = NULL,
                              isPer = FALSE){

  # # Función para calcular la varianza del promedio de la escala.
  # #   Realiza estimación por dominios.
  # #
  # # Arg:
  # #  data[data.frame]: data frame que contenga las variables agrega,
  # #                    y, dominio, upm, use, peso y matricula.
  # #  agrega[character]: nombre de la variable que define los niveles
  # #                     para el cálculo del error
  # #  y[character]: nombre de la variable que define la variable
  # #                     para la cual se cálculan los errores
  # #  dominio[character]: nombre de la variable que corresponde al dominio,
  # #                      si no se desean resultados por dominios toma el
  # #                      valor NULL
  # #  isPer[logical]: si TRUE se desea calcular errores de porcentajes
  # #                  si FALSE se desea calcular errores de promedios
  # #
  # # Ret:
  # #  data.frame con la estimación de varianza por
  # #  categoría de agrega y por dominio (si aplica)

  varKeep <- c(agrega, 'NoHoja', 'establecimiento', 'sejoId', y,
               'grupo', dominio, 'n')

  data    <- na.omit(data[, varKeep])

  data    <- mutate(data, sejoGp = paste(sejoId, grupo, sep = ''))

  if(!is.null(dominio)){
     cat           <- levels(as.factor(data[, dominio]))
     data[, 'dom'] <- data[, dominio] == cat[1]
  }else{
     data          <- mutate(data, dom = 1)
  }

 # # Funcion que calcula los errores

 CalErrCen <- function(data, agrega, y, dominio){

 # # Calculo del tamaño de muestra y de la matricula

     matri    <- ddply(data, .(sejoGp), summarise, nSam = length(unique(NoHoja)) )

     data     <- merge(data, matri, by = 'sejoGp', all.x = TRUE)

 # # Calculo de cantidades

     if(!is.null(dominio)){
        dataDom  <- data[data[, 'dom'], ]
     }else{
        dataDom  <- data
        dominio  <- 'dom'
     }

     fun <- function(x){ partes <- data.frame(
                      ni                 = as.numeric(min(x[, 'nSam'])),
                      Ni                 = as.numeric(min(x[, 'n'])),
                      ybsdi              = mean(x[, y], na.rm = TRUE),
                      y2bsdi             = mean(x[, y]^2, na.rm = TRUE)
                      )
                      partes[, 'Pi']     = length(x[, dominio])/partes[, 'ni']
                      partes[, 'fi']     = partes[, 'ni']/partes[, 'Ni']
                      partes[, 'ybdsi']  = sum(x[, y], na.rm=TRUE)/partes[, 'ni']
                      partes[, 'y2bdsi'] = sum(x[, y]^2, na.rm=TRUE)/partes[, 'ni']
                      partes[, 'NiPi']   = partes[, 'Ni']*partes[, 'Pi']
        return(partes)
      }

      cantidades <- ddply(dataDom, c(agrega, 'establecimiento', 'sejoGp'),
                      .fun = fun )

      ymd        <- ddply(cantidades, agrega, summarise,
                      ymd = sum(Ni*ybsdi*Pi, na.rm = TRUE)/sum(Ni*Pi, na.rm = TRUE))

      cantidades <- merge(cantidades, ymd, by = 'municipio', all.x = TRUE)

      sumandos   <- ddply(cantidades, c(agrega, "establecimiento", "sejoGp"),
                         summarise,
                         NiPi   = NiPi,
                         f      = (Ni^2)*(1-fi)/(ni*(ni-1)),
                         parte1 = ni*Pi*(y2bsdi - ybsdi^2),
                         parte2 = ni*Pi*(1-Pi)*(ybsdi - ymd)^2,
                         cmpVm  = f*(parte1 + parte2) )

       var <- ddply(sumandos, c(agrega), summarise, varianza =
                 sum(cmpVm, na.rm = TRUE)/sum(NiPi, na.rm = TRUE)^2 )

       return(var)

 }

  if(isPer){

     niveles   <- levels(data[, y])
     clases    <- paste(y, niveles, sep = '')
     baseDummy <- dummy.data.frame(y, data = data)

     resultList     <- list()
     resultListComp <- list()
     for(kk in clases){
         if(!is.null(dominio)){
            resultList[[kk]] <- CalErrCen(baseDummy, agrega, kk, 'dom')
            baseDummy[, 'dom']   <- data[, dominio] == cat[2]
            resultListComp[[kk]] <- CalErrCen(baseDummy, agrega, kk, 'dom')
         }else{
            resultList[[kk]] <- CalErrCen(baseDummy, agrega, kk, NULL)
         }
     }

     result            <- ldply(resultList)
     result            <- reshape(result, idvar = agrega, timevar = ".id",
                                  direction = "wide")
     names(result)     <- gsub(paste('varianza.', y, sep = ''), '',
                                      names(result))

     if(!is.null(dominio)){
        resultComp        <- ldply(resultListComp)
        resultComp        <- reshape(resultComp, idvar = agrega, timevar = ".id",
                                  direction = "wide")
        names(resultComp) <- gsub(paste('varianza.', y, sep = ''), '',
                               names(resultComp))

        result[, dominio] <- cat[1]
        resultComp[, dominio] <- cat[2]

        resultados <- rbind(result, resultComp)
        resultados <- resultados[order(resultados[, agrega]), ]
        resultados <- resultados[, c(agrega, dominio, niveles)]
     }else{
        resultados <- result
     }

  }else{

     resultados     <- CalErrCen(data, agrega, y, dominio)

     if(!is.null(dominio)){
        data[, 'dom']     <- data[, dominio] == cat[2]
        resultComp        <- CalErrCen(data, agrega, y, 'dom')
        resultados        <- list(resultados, resultComp)
        names(resultados) <- cat
        resultados <- ldply(resultados, .id = dominio)
        resultados <- resultados[order(resultados[, agrega]), ]
        resultados <- resultados[, c(agrega, dominio, 'varianza')]
     }
  }

 return(resultados)
}

