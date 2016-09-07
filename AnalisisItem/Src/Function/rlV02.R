# Calcular DIF (RL)
################################################################################
# #
# # Metodos e Instrumentos para Investigacion en Salud - MIIS
# # Proyecto Sesgo cultural en el examen de estado ICFES
# #
# # Deteccion de DIF - Dos etapas
# # Purificacion de medida de equiparacion y reporte de items que presentan DIF
# # Procedimiento de R-L
# # Uso de pseudo-R2 como medidas de tamano del efecto
# #
# # Script para R
# #
# # Victor H Cervantes
# # vhcervantesb@unal.edu.co
# #
# # Last revision: Feb 10, 2009
# #
################################################################################

################################################################################
# # Funciones generales para el procedimiento
################################################################################

# # Crear variable de equiparacion basada en el func_puntaje en la prueba
func_puntaje <- function(base, ...) {
  omit <- c(...)
  if (length(omit) > 0) {
    apply(base[,-omit], 1, sum, na.rm=TRUE)
  } else {
    apply(base, 1, sum, na.rm=TRUE)
  }
}

# # Standardizing function
func_z <- function(variable){
  ( (variable - mean(variable, na.rm=TRUE)) / sd(variable, na.rm=TRUE) )
}

 
###############################################################################
# # Funciones para obtener las regresiones logisticas de los 3 modelos anidados
###############################################################################

# # Modelos de regresion logistica
func_RL_crit <- function(resp_var, crit, use.glm = TRUE, use.nnet = FALSE, ...){
  if (use.glm == TRUE & use.nnet == FALSE) {
    rl <- glm(resp_var ~ func_z(crit), binomial, ...)
  } 
  if(use.glm == FALSE & use.nnet == FALSE) {
    require(rms)
    rl <- lrm(resp_var ~ func_z(crit), ...)
    rl$y             <- resp_var
    rl$null.deviance <- rl$deviance[1]
    rl$deviance      <- rl$deviance[2]
    rl$fitted.values <- 1 / (1 + exp(-predict(rl)))
    rl$df.residual   <- rl$stats[["d.f."]]
  }
  if(use.glm == FALSE & use.nnet == TRUE){
    require(nnet)
    rl <- multinom(resp_var ~ func_z(crit))
    rl$df.residual <- rl$edf
    nullrl <- multinom(resp_var ~ 1)
    rl$null.deviance <- nullrl$deviance
  }
  if(use.glm == TRUE & use.nnet == TRUE){
    stop("Stop procedure due to specify glm and multinom model at the same")
  }  
  rl
}

func_RL_no_inter <- function(resp_var, crit, group, use.glm = TRUE, 
                             use.nnet = FALSE, ...) {
  if (use.glm == TRUE & use.nnet == FALSE) {
  rl <- glm(resp_var ~ func_z(crit) + func_z(group), binomial, ...)
  } 
  if (use.glm == FALSE & use.nnet == FALSE){
    require(rms)
    rl <- lrm(resp_var ~ func_z(crit) + func_z(group), ...)
    rl$y             <- resp_var
    rl$null.deviance <- rl$deviance[1]
    rl$deviance      <- rl$deviance[2]
    rl$fitted.values <- 1 / (1 + exp(-predict(rl)))
    rl$df.residual   <- rl$stats[["d.f."]]
  }
  if(use.glm == FALSE & use.nnet == TRUE){
  require(nnet)    
  rl <- multinom(resp_var ~ func_z(crit)+ func_z(group), ...)
  rl$df.residual <- rl$edf
  nullrl <- multinom(resp_var ~ 1)
  rl$null.deviance <- nullrl$deviance
  }  
  if(use.glm == TRUE & use.nnet == TRUE){
    stop("Stop procedure due to specify glm and multinom model at the same")
    }  
  rl
}

func_RL_inter <- function(resp_var, crit, group, use.glm = TRUE, 
                          use.nnet = FALSE, ...) {
  if (use.glm == TRUE & use.nnet == FALSE) {
  rl <- glm(resp_var ~ func_z(crit) * func_z(group), binomial, ...)
  } 
  if (use.glm == FALSE & use.nnet == FALSE) {
    require(rms)
    rl <- lrm(resp_var ~ func_z(crit) * func_z(group), ...)
    rl$y             <- resp_var
    rl$null.deviance <- rl$deviance[1]
    rl$deviance      <- rl$deviance[2]
    rl$fitted.values <- 1 / (1 + exp(-predict(rl)))
    rl$df.residual   <- rl$stats[["d.f."]]
  }

  if(use.glm == FALSE & use.nnet == TRUE){
  require(nnet)    
  rl <- multinom(resp_var ~ func_z(crit)* func_z(group), ...)
  rl$df.residual <- rl$edf
  nullrl <- multinom(resp_var ~ 1)
  rl$null.deviance <- nullrl$deviance  
  }
  if(use.glm == TRUE & use.nnet == TRUE){
    stop("Stop procedure due to specify glm and multinom model at the same")
    }
  rl
}


###############################################################################
# # Funciones para calcular los diferentes R2
###############################################################################

func_R_McFadden <- function(rl) {
  num <- rl$deviance
  den <- rl$null.deviance 
  1 - (num/den)
}


###############################################################################
# # Obtencion de los modelos
###############################################################################

# # Calcular Regresiones-Logisticas sobre los items de la base
func_RL_DIF <- function(base, crit, group, tipo = "conjunto", 
                        use.glm = TRUE, use.nnet = FALSE, ...) {
  if (tipo == "conjunto") {
    cbind(lapply(base, func_RL_crit, crit, use.glm, use.nnet, ...),
          lapply(base, func_RL_inter, crit, group, use.glm, use.nnet, ...))
  } else {
    cbind(lapply(base, func_RL_crit, crit, use.glm, use.nnet, ...),
        lapply(base, func_RL_no_inter, crit, group, use.glm, use.nnet, ...),
          lapply(base, func_RL_inter, crit, group, use.glm, use.nnet, ...))
  }
}

###############################################################################
# # Funciones para identificacion de items con DIF segun RL
###############################################################################

# # Prueba de hipotesis sobre los modelos (Chi2, Deviance)

modeltest <- function(coupled_data, tipo = "conjunto") {
# Input coupled data should be as a line resulting from
# func_RL_DIF
  if (tipo == "conjunto") {
    LL <- abs(coupled_data[[1]][["deviance"]] - coupled_data[[2]][["deviance"]])
    df <- abs(coupled_data[[1]][["df.residual"]] - coupled_data[[2]][["df.residual"]])
    c(G = LL, df = df, p.value = pchisq(LL, df, lower.tail = FALSE))
  } else {
    LL1 <- abs(coupled_data[[1]][["deviance"]] - coupled_data[[2]][["deviance"]])
    LL2 <- abs(coupled_data[[2]][["deviance"]] - coupled_data[[3]][["deviance"]])

    df1 <- abs(coupled_data[[1]][["df.residual"]] - coupled_data[[2]][["df.residual"]])
    df2 <- abs(coupled_data[[2]][["df.residual"]] - coupled_data[[3]][["df.residual"]])
    
    c(G1 = LL1, df1 = df1, p.value1 = pchisq(LL1, df1, lower.tail = FALSE),
      G2 = LL2, df2 = df2, p.value2 = pchisq(LL2, df2, lower.tail = FALSE))
  }
}


# # Obtener las pruebas de Chi2 para todos los items
func_RL_test <- function(RL_DIF, tipo = "conjunto") {
    apply(RL_DIF, 1, modeltest, tipo)
}

# # Items para los cuales la prueba es significativa
func_RL_sig <- function(RL_test, alfa_level, tipo = "conjunto") {
  if (tipo == "conjunto") {
    identified <- row(as.matrix(RL_test["p.value", ]) )[RL_test["p.value", ] < alfa_level]
  } else {
    identified1 <- row(as.matrix(RL_test["p.value1", ]) )[RL_test["p.value1", ] < alfa_level]
    identified2 <- row(as.matrix(RL_test["p.value2", ]) )[RL_test["p.value2", ] < alfa_level]
    identified <- unique( c(identified1, identified2) )
  }
  return(identified)
}

###############################################################################
# # Funciones para calcular el R Delta Cuadrado -de McFadden
###############################################################################


# # Obtener R2 de McFadden
func_R_Delta_McFadden <- function(coupled_data, tipo = "conjunto") {
  if (tipo == "conjunto") {
	  R1 <- func_R_McFadden(coupled_data[[1]])
	  R3 <- func_R_McFadden(coupled_data[[2]])
	  Rs <- list(R2crit = R1, R2inter = R3)
	  Rdelta <- list(RdG = R3 - R1)
  } else {
	  R1 <- func_R_McFadden(coupled_data[[1]])
	  R2 <- func_R_McFadden(coupled_data[[2]])
	  R3 <- func_R_McFadden(coupled_data[[3]])
	  Rs     <- list(R2crit = R1, R2group = R2, R2inter = R3)
	  Rdelta <- list(RdU = R2 - R1, RdN = R3 - R2)
  }
  return(list(Rs, Rdelta))
}


func_R_Deltas_McFadden <- function(RL_DIF, tipo = "conjunto") {
  R2MF <- apply(RL_DIF, 1, func_R_Delta_McFadden, tipo)
  return(R2MF)
}



###############################################################################
# # Funciones para ejecutar las dos fases en la identificacion
# # basadas en el puntaje en la prueba
# # detectados tanto al nivel de 0.05 como 0.01 de significancia
# # empleando y sin emplear una medida del tamaño del efecto (R2delta)
###############################################################################

# # Obtener los items detectados con DIF en la primera fase
func_RL_fase_1 <- function(base, group, alfa, tipo = "conjunto", 
                           use.glm = TRUE, use.nnet = FALSE, itSha) {

  RL_DIF <- func_RL_DIF(base[, itSha], func_puntaje(base), group, tipo, use.glm, use.nnet)
  tests  <- func_RL_test(RL_DIF, tipo)
  sig    <- func_RL_sig(tests, alfa, tipo)

  nitems <- ncol(base)
  if ( length(sig) >= (nitems - 2) ) {
  	sig <- c()
  }

  salida  <- list(sig = sig)

  return(salida)
}

# # Obtener los items detectados con DIF en la segunda fase
# # junto con sus caracteristicas

func_RL_fase_2 <- function(base, group, fase_1, tipo = "conjunto", 
                           use.glm = TRUE, use.nnet = FALSE, pseudo.R = "McFadden", itSha) {

  items_fase_1 <- fase_1$sig
  Rdelta  <- switch(pseudo.R, McFadden = func_R_Deltas_McFadden, None = NULL)

  nomIt_fase1 <- names(base[, itSha])[items_fase_1]
  basePunt <- base[, !names(base) %in% nomIt_fase1]

  RL_DIF_2nd  <- func_RL_DIF(base[, itSha], func_puntaje(basePunt),
                             group, tipo, use.glm, use.nnet)
  tests_2nd   <- func_RL_test(RL_DIF_2nd, tipo)


  if (pseudo.R == "McFadden") {
    Rdeltas_2nd <- Rdelta(RL_DIF_2nd, tipo)

    R.sq   <- data.frame(lapply(Rdeltas_2nd, function(x) unlist(x[[1]])))
    Deltas <- data.frame(lapply(Rdeltas_2nd, function(x) unlist(x[[2]])))
      
    colnames(R.sq) <- colnames(tests_2nd)
    names(Deltas)  <- colnames(tests_2nd)
  }

  if (tipo == "conjunto") {
    tests <- c(1, 3)
  } else {
    tests <- c(1, 3, 4, 6)
  }
  
  coefExtract <- function(x, use.nnet) {
                 if(!use.nnet) {
                  return(x[["coefficients"]])
                } else {
                  z <- t(coef(x))
                  return(z)
                }
          }

  if(!use.nnet){
    coeff <- as.data.frame(lapply(RL_DIF_2nd[,ncol(RL_DIF_2nd)],
                         coefExtract, use.nnet = use.nnet))
  } else {
    oneP <- lapply(RL_DIF_2nd[,ncol(RL_DIF_2nd)], coefExtract, use.nnet = use.nnet)
    for(ii in 1:length(oneP)){
        colnames(oneP[[ii]]) <- paste(names(oneP[ii]), colnames(oneP[[ii]]), sep ="")
        }
    coeff <- data.frame(do.call("cbind", oneP))
  }

  if(!use.nnet){
    colnames(coeff) <- colnames(tests_2nd)  
    RLDIF <- rbind(tests_2nd[tests,], coeff)

    if (pseudo.R != "None") {
      RLDIF <- rbind(RLDIF, R.sq, Deltas)
      if (length(tests) == 2) {
        rownames(RLDIF)[7:9] <- c("R1", "R3", "DRC")
      } else {
        rownames(RLDIF)[9:13] <- c("R1", "R2", "R3", "DRU", "DRN")
      }
    }
  } else {
    RLDIF <- tests_2nd[tests,]

    if (pseudo.R != "None") {
       RLDIF <- rbind(RLDIF, R.sq, Deltas)
       if (length(tests) == 2) {
        rownames(RLDIF)[3:5] <- c("R1", "R3", "DRC")
      } else {
        rownames(RLDIF)[5:9] <- c("R1", "R2", "R3", "DRU", "DRN")
      }
    }
     
    RLDIF <- list(coef = coeff, testDIF = RLDIF)
  }
  return(RLDIF)
}


func_RL_ambas_fases <- function(base, group, tipo = "conjunto", 
                                use.glm = TRUE, use.nnet = FALSE, 
                                pseudo.R = "None", itSha) {

detected <- func_RL_fase_1(base, group, 0.01, "conjunto", use.glm, use.nnet, itSha)
func_RL_fase_2(base, group, detected, tipo, use.glm, use.nnet, pseudo.R, itSha)
}

