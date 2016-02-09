################################################################################
# # readFilesAI.R
# # R Versions: R version 2.12.0
# #
# # Author(s): Alvaro Uzaheta
# #
# # SABER 5 y 9
# # Description: Functions to read files FA
# #
# # Inputs: A file folder with the structure that AnalItem gives
# #
# # Outputs: A data frame with the information decoded
# #
# # File history:
# #   20111116: Creation
# #   20111121: The function is available for categorical variables
# #   20111122: Implementation of orientation of variables and variables with 'O'
# #             how valid value
# #   20111130: Include the option of collapse categories
# #   20111202: revision of collapse categories and inclusion of a new parameter to
# #             excluded eliminated variables from lecture
# #   20120201: Include the option to change variables according to
# #             conditional variables
# #   20121126: Changes to considere the structure from AnalItem
# #   20121129: The changes for conditional structures are dropped
# #   20130426: Include library LaF to read data set
# #   20141102: Review 'no present' items
# #   20142702: Include function "datPrueba" to generate datBlock (data by proof)
# #             from datBlockF (data by form)
# #   20140311: Function "ReadDataFA" is corrected for reading data from variables
# #             with indirect guidance, and ordering the dictionary by
# #             variables "hoja" and "order" to evite dictionary to
# #             input ordered
# #   20140312: Include function "ReadDataFAColapsa" to collapse
# #             datBlock according to the new dictionary by subject
# #
# # ToDo:
# #       Include the option for continuos variables in the form
# #       Include the option that some categories aren't use
# #       Include the use of orden of categories besides the natural
# #         order of values
################################################################################

################################################################################
  # # function to perform checks
################################################################################
  CheckMiss <- function (arg1, arg2) {
    # # reports the values missing in arg2 that are in arg1
    # #
    # # Arg:
    # #  arg1: [vector] a vector of values to check belong to arg2
    # #  arg2: [vector] a vector of values
    # #
    # # Ret:
    # #  print the values that are not present in arg2 that are in arg1
    miss <- arg1[!(arg1 %in% arg2)]

    return(paste(miss, collapse = '\n'))
    cat(match.call())
  }

################################################################################
# #  reading of dictionary
################################################################################
ReadDict <- function (fileName, variables, categories, model, index,
                      collapse = NULL, desElim = NULL) {
  # # This function read excel sheets with general information about the
  # # items and characteristics to take account in the decoded
  # # procedure, performs some validations to ensure consistency of
  # # information
  # #
  # #
  # # Arg:
  # #   fileName:   [character] name of the file with the tables of the
  # #               dictionary, expected by a .xlsx file in the
  # #   variables:  [character] name of the sheet in the excel file with
  # #               the information of the items and additional
  # #               information of them
  # #   categories: [character] name of the sheet in the excel file with
  # #               the information of the categories of the items and
  # #               the description of the type of variable
  # #   model:      [character] name of the sheet in the excel file with
  # #               the information of the models use for the indexes
  # #   index:      [character] name of the sheet in the excel file with
  # #               the information of the index with a short
  # #               description of them
  # #   collapse:   [character] name of the sheet in the excel file with
  # #               the information of the collapse categories in some
  # #               items
  # #   desElim:    [character] name of the sheet in the excel file with
  # #               the description of possibles reasons to eliminate
  # #               variables
  # #
  # # Ret:
  # #   dict: [list] a list with the dictionary of variables
  # #         (dict$variables), the dictionary of categories
  # #         (dict$categories), the dictionary of index (dict$index),
  # #         the dicitonary of models (dict$model), the categories
  # #         collapse (dict$collapse) and the description of reasons to
  # #         eliminate variables (dict$desElim)


  ################################################################################
  # # verification of parameters
  ################################################################################
  nameSheets <- c(variables, categories, model, index, collapse, desElim)
  arguments <- c(fileName, nameSheets)
  if (!all(is.character(arguments))) {
    stop('All the arguments need to be character or NULL for the\n',
         '    collapse\n    desElim\n',
         'Arguments')
  }

  if (any(sapply(list(variables, categories, model, index, fileName), is.null))) {
    stop('Any of the follow parameter is NULL:\n  variables\n  categories\n',
         '  model\n  index\n  fileName\nThey are necessary for the function')
  }

  if (!file.exists(fileName)) {
    stop("The fileName = ", fileName, " doesn't exists")
  }

  ################################################################################
  # # lecture of the files from the excel file
  ################################################################################
  require(RODBC)

  sheets <- paste(nameSheets, "$", sep = "")

  channel <- odbcConnectExcel2007(fileName)
  tables <- sqlTables(channel)
  # # verification that the tables ask for reading exist
  if (!all(sheets %in% tables$TABLE_NAME)) {
    close(channel)
    stop("Some tables are not found in the file: \n",
         CheckMiss(sheets, tables$TABLE_NAME))
  }

  dictionary <- list()
  dictionary$variables  <- sqlFetch(channel, variables,   as.is = TRUE)
  cat("Number of items on the table of variables = \n     ",
     nrow(dictionary$variables), "\n", sep = "")

  dictionary$categories <- sqlFetch(channel, categories,  as.is = TRUE)
  dictionary$model      <- sqlFetch(channel, model,       as.is = TRUE)
  dictionary$index      <- sqlFetch(channel, index,       as.is = TRUE)
  if (!is.null(collapse)) {
    dictionary$collapse   <- sqlFetch(channel, collapse,    as.is = TRUE)
  }
  if (!is.null(desElim)) {
    dictionary$desElim    <- sqlFetch(channel, desElim,     as.is = TRUE)
  }
  close(channel)


  ################################################################################
  # # verification that some required variables exist
  ################################################################################
  fieldsVar <- c("id", "opRes", "orden", "orient", "codMod", "elimina", "colapsa")
  fieldsCat <- c("opRes", "valor", "etiqueta", "tipo", "orden")
  fieldsMod <- c("codMod", "model")
  fieldsInd <- c("indice", "dimension", "tema", "nombre")
  fieldsCol <- c("id", "colAnt", "colNueva", "labAnt", "labNuevo")
  fieldsEli <- c("elimina", "descripcion")

  if (!all(fieldsVar %in% names(dictionary$variables))) {
    stop("some fields required in the dictionary of variables are missing: ",
          CheckMiss(fieldsVar, names(dictionary$variables)), "\n")
  }
  if (!all(fieldsCat %in% names(dictionary$categories))) {
    stop("some fields required in the dictionary of categories are missing: ",
          CheckMiss(fieldsCat, names(dictionary$categories)), "\n")
  }
  if (!all(fieldsMod %in% names(dictionary$model))) {
    stop("some fields required in the dictionary of model are missing: ",
          CheckMiss(fieldsMod, names(dictionary$model)), "\n")
  }
  if (!all(fieldsInd %in% names(dictionary$index))) {
    stop("some fields required in the dictionary of index are missing: ",
          CheckMiss(fieldsInd, names(dictionary$index)), "\n")
  }
  if (!is.null(collapse) & !all(fieldsCol %in% names(dictionary$collapse))) {
    stop("some fields required in the dictionary of index are missing: ",
          CheckMiss(fieldsCol, names(dictionary$collapse)), "\n")
  }
  if (!is.null(desElim) & !all(fieldsEli %in% names(dictionary$desElim))) {
    stop("some fields required in the dictionary of desElim are missing: ",
          CheckMiss(fieldsEli, names(dictionary$desElim)), "\n")
  }

  # # verification of ..
  if (!all(dictionary$variables[, "opRes"] %in% dictionary$categories[, "opRes"]) |
      !all(dictionary$categories[, "opRes"] %in% dictionary$variables[, "opRes"])) {
    varOpRes <- unique(dictionary$variables[, "opRes"])
    catOpRes <- unique(dictionary$categories[, "opRes"])
    if (!all(varOpRes %in% catOpRes)) {
      cat("# # Warning:
          Some values for opRes in the variables dictionary don't
          exist in the categories dictionary:\n",
          CheckMiss(varOpRes, catOpRes), "\n")
    }
    if (!all(catOpRes %in% varOpRes)) {
      cat("# # Warning:
          Some values for opRes in the categories dictionary don't
          exist in the variables dictionary:\n",
          CheckMiss(catOpRes, varOpRes), "\n")
    }
  }

  if (!is.null(collapse)) {
    isCollapse <- dictionary$variables[, "colapsa"] == "01"
    varOpRes <- unique(dictionary$variables[isCollapse, "id"])
    catOpRes <- unique(dictionary$collapse[, "id"])
    if (!all(varOpRes %in% catOpRes)) {
      cat("# # Warning:
          Some values for id in the variables dictionary don't
          exist in the collapse dictionary\n:",
          CheckMiss(varOpRes, catOpRes), "\n")
    }
    if (!all(catOpRes %in% varOpRes)) {
      cat("# #Warning:
          Some values for id in the collapse dictionary don't
          exist in the variables dictionary:\n",
          CheckMiss(catOpRes, varOpRes), "\n")
    }
    if (!all(dictionary$variables[, "colapsa"] %in% c("00", "01"))){
      cat("# #Warning:
          Some items have a not possible value for colapsa\n")
    }
  }

  if (!all(dictionary$variables[, "indice"] %in% dictionary$index[, "indice"])
      |
      !all(dictionary$index[, "indice"] %in% dictionary$variables[, "indice"])) {
    varOpRes <- unique(dictionary$variables[, "indice"])
    catOpRes <- unique(dictionary$index[, "indice"])
    if (!all(varOpRes %in% catOpRes)) {
      cat("# # Warning:
          Some values for indice in the variables dictionary don't
          exist in the index dictionary:\n",
          CheckMiss(varOpRes, catOpRes), "\n")
    }
    if (!all(catOpRes %in% varOpRes)) {
      cat("# # Warning:
          Some values for 'indice' in the index dictionary don't
          exist in the variables dictionary:\n",
          CheckMiss(catOpRes, varOpRes), "\n")
    }
  }

  if (!all(dictionary$variables[, "codMod"] %in% dictionary$model[, "codMod"])
      |
      !all(dictionary$model[, "codMod"] %in% dictionary$variables[, "codMod"])) {
    varOpRes <- unique(dictionary$variables[, "codMod"])
    catOpRes <- unique(dictionary$model[, "codMod"])
    if (!all(varOpRes %in% catOpRes)) {
      cat("# # Warning:
          Some values for codMod in the variables dictionary don't
          exist in the model dictionary:\n",
          CheckMiss(varOpRes, catOpRes), "\n")
    }
    if (!all(catOpRes %in% varOpRes)) {
      cat("# # Warning:
          Some values for codMod in the model dictionary don't
          exist in the variables dictionary:\n",
          CheckMiss(catOpRes, varOpRes), "\n")
    }
  }

  if (!all(dictionary$variables[, "orient"] %in% c("DIRECTA", "INVERSA",
                                                   "INDIRECTA"))){
    cat("# # Warning:
        Some items have a not possible value for orientation\n")
  }

  if (!is.null(desElim)) {
    varOpRes <- unique(dictionary$variables[, "elimina"])
    catOpRes <- unique(dictionary$desElim[, "elimina"])
    if (!all(varOpRes %in% catOpRes)) {
      cat("# # Warning:
          Some values for elimina in the variables dictionary don't
          exist in the desElim dictionary:\n",
          CheckMiss(varOpRes, catOpRes), "\n")
    }
  }

  # # delete line break
  dictionary$variables[, ]  <- lapply(dictionary$variables,
                                      function (x) gsub("\n", "", x))



  attr(dictionary, "call") <- list(collapse = collapse, desElim = desElim)

  class(dictionary) <- "RD"

  cat("# # Results:\n")
  str(dictionary, max.level = 1)


# #   return(dictionary)
  structure(dictionary, call = list(collapse = collapse, desElim = desElim),
            class = "RD", parameters = match.call(),
            features = c(fileName = fileName, variables = variables) )
}


################################################################################
# # function to read data files
################################################################################

ReadDataFA <- function (df, dict, path = inPath, noOmission = TRUE, omissionThreshold = 0.5,
                      multiMarkOmiss = TRUE, verbose = TRUE, eliminatedVars = TRUE,
                      questionnaire = "FA", Block = "F15") {
  # # Function to read data files with lecture of B sheets from the input
  # #
  # # Arg:
  # #   fileName: [character] name of the file of lecture, in general the shape is BXX.txt where
  # #         XX is the number of the form that you want read
  # #   dict: [list] a list with the dictionary of variables (dict$variables), the dictionary
  # #         of categories (dict$categories) and the dictionary of index (dict$index)
  # #   path: [character] path where the directory is ubicated, default inPath
  # #   noOmission: [logical] (TRUE or FALSE). Should rows with more than omissionThreshold missing values delete?
  # #   omissionThreshold: [numeric] threshold to delete cases with more missing values
  # #   multiMarkOmiss: [logical] (TRUE or FALSE). Should multi mark and omission values
  # #         include as levels of the variables?
  # #   verbose: [logical] (TRUE or FALSE). If TRUE, then tables for each variable are
  # #         printed, shown the final label against the character in lecture
  # #   eliminatedVars: [logical] (TRUE or FALSE). IF TRUE eliminated Vars in the dictionary are
  # #         read in the return data.frame, if FALSE eliminated vars are omitted in the return data.frame
  # #   questionnaire: [character] which part of the questionnaire is taken for analysis
  # #
  # # Ret:
  # #  [data.frame] that has the variables with the characteristics that apper in
  # #         dict list and the variables for identification of the number of sheet

  ################################################################################
  # # required libraries
  ################################################################################
  require(car)

  ################################################################################
  # # validate parameters
  ################################################################################
  if (omissionThreshold <0 | omissionThreshold > 1) {
    stop("The value for omissionThreshold is not between 0 and 1")
  }

  ################################################################################
  # # general settings of the function
  ################################################################################
  read <- df
  cat("Number of rows read:", dim(read)[1], "\n")
  read[, c("string")] <- gsub(" ", "",  read[, c("string")])

  # # if noOmission is true report the number of rows deleted
  omission <-  nchar(gsub("O", "", read[, c("string")]))
  cat("Rows with only omission character:", sum(omission == 0),"\n")
  read <- read[omission != 0, ]
  cat("Rows after delete only omission character:", dim(read)[1],"\n")

  # # comparision of the number if items in dictionary and in the input file
  dict$variables[, "orden"] = as.numeric(dict$variables[, "orden"])
  dict$variables <- dict$variables[order(dict$variables[,
                                         "hoja"],dict$variables[, "orden"]), ]
  itemsBlock <- grep(Block, dict$variables[, "hoja"])
  cat("Number of items in dictionary: ", length(itemsBlock), ". Number of character in string: ",
      unique(nchar(read[, c("string")])), "\n", sep = "")
  itemsCont <- grep("continua", dict$variables[,
                                               "opRes"][dict$variables[, "hoja"] == Block])
  if (length(itemsCont) != 0) {
    itemsAdi <- length(itemsCont) *
      dict$categories[dict$categories[, "opRes"] == "continua",
                      "orden"] - length(itemsCont)
    itemsTot <- length(itemsBlock) + itemsAdi
  } else  itemsTot <- length(itemsBlock)
  if (itemsTot != unique(nchar(read[, c("string")]))) {
    stop("\n\nERROR: Number of variables is not equal to the number of characteres in string")
  }
  # # the function assumes that the dictionary of variables is sort in the same order that characters
  fieldsVar <- c("id", "opRes", "orden", "orient", "codMod", "colapsa", "elimina", "prueba")
  fieldsCat <- c("opRes", "valor", "etiqueta", "tipo")
  if (!all(fieldsVar %in% names(dict$variables))) {
    stop("some fields required in the dicitonary of variables are missing: ",
         fieldsVar[!(fieldsVar %in% names(dict$variables))])
  }
  if (!all(fieldsCat %in% names(dict$categories))) {
    stop("some fields required in the dicitonary of categories are missing: ",
         fieldsCat[!(fieldsCat %in% names(dict$categories))])
  }

  ################################################################################
  # # creation of the data.frame with the variables report in the dictionary
  ################################################################################

  if (verbose) {
    cat("\n verbose = TRUE, labels againts string in \n")
  }
  base <- read[, grep("str", names(read), invert = TRUE)]

  # # creation of table with one column for each variable
  posiNo <- 1 ## useful for continous variables with more then one character

  for (ii in itemsBlock) {
    variables   <- dict$variables[ii, fieldsVar]
    categories  <- subset(dict$categories, opRes %in% variables[, "opRes"])
    nameItem    <- variables[, "id"]
    typeItem    <- unique(categories[, c("tipo")])
    collapse    <- subset(dict$collapse, id == nameItem)
    nrowCollap  <- nrow(collapse)
    isCollapse  <- nrowCollap > 0

    isEliminate <- variables[, "elimina"] != "06"

    # # omitted all the lecture of the file if the var is eliminated
    if (eliminatedVars & isEliminate) {
      cat("# #",
          nameItem, "was eliminated own due",
          dict$desElim[dict$desElim[, "elimina"] == variables[, "elimina"], "descripcion"],
          "It's omitted from the final data.frame return by the function\n",
          "# #\n\n")
      posiEnd <- posiNo
    } else {
      if (is.na(variables[, "orient"])) {
        cat("For: ", nameItem, " is missing the value of orientation, this is assume to be direct\n", sep = "")
        descItem <- FALSE
      } else if (!(variables[, "orient"] %in% c("DIRECTA", "INVERSA"))) {
        cat("Orientation of the variables '", as.character(variables[, "orient"]),
            "' is not between the types availables, this is assume to be direct\n", sep = "")
        descItem <- FALSE
      } else {
        descItem <- substr(variables[, "orient"], 1, 1) == 'I'
      }

      # # check if the categories have an unique type
      if (length(typeItem) > 1) {
        stop("Type of variable ", nameItem, " is not unique")
      }
      if (!(typeItem %in% c("Continua", "Nominal", "Ordinal"))) {
        stop("The type of the item '", typeItem, "' is not between the types availables")
      }
      # # statements by type of the variable
      if (typeItem == "Continua") {
        cat("Lecture variable ", nameItem, "\n")
        posiEnd  <- posiNo + categories[, "orden"] - 1
        souItemPrev <- substr(read[, "string"], posiNo, posiEnd)
        print(table(souItemPrev, useNA = 'ifany'))
        # # recoding the wrong filling out
        souItemMod  <-  gsub("(\\d)OO", "\\1", souItemPrev)
        souItemMod  <-  gsub("[O0](\\d)[O0]", "0\\10", souItemMod)
        souItemMod  <-  gsub("(\\d{2})O", "\\1", souItemMod)
        souItemMod  <-  gsub("O", "", souItemMod)
        cat("Summary of continuos variable \n")
        varItem    <- as.integer(souItemMod)
        base[, nameItem] <- varItem
        print(summary(base[, nameItem]))
        #posiNo <- posiEnd + 1
      } else {
        posiEnd <- posiNo
        souItemPrev <- substr(read[, "string"], posiNo, posiEnd)

        if (isCollapse) {
          recoding <- paste("'", collapse[, "colAnt"], "'='", collapse[, "colNueva"], "'", sep = "")
          recodes <- recoding[1]
          if (nrowCollap > 1) {
            for (ll in 2:nrowCollap) {
              recodes <- paste(recodes, recoding[ll], sep = ";")
            }
          }
          cat(nameItem, "was collapse with", recodes, "\n")
          souItem <- recode(souItemPrev, recodes, as.numeric.result = FALSE, as.factor.result = FALSE)
        } else {
          souItem <- souItemPrev
        }

        missingValues <- c("O", "M")
        missingLabel  <- c("NR", "Multimarca")
        labelItem <- as.character(categories[order(categories[, "valor"], decreasing = descItem), "etiqueta"])
        levelItem <- categories[order(categories[, "valor"], decreasing = descItem), "valor"]
        missingLabel  <- missingLabel[!(missingValues %in% levelItem)]
        missingValues <- missingValues[!(missingValues %in% levelItem)]

        if (multiMarkOmiss) {
          varItem   <- factor(souItem)
          labelItem <- c(missingLabel, labelItem)
          levelItem <- c(missingValues, levelItem)
        } else {
          varItem   <- factor(souItem, exclude = missingValues)
        }
        if (typeItem == "Nominal") {
          base[, nameItem] <-  factor(varItem, levels = levelItem, labels = labelItem)
        } else {
          orden <- c(1, 2, categories[, "orden"] + 2)
          valores <- levelItem
          ordenDF <- data.frame(valores, orden, stringsAsFactors = FALSE)
          levelItemDF <- data.frame(levelItem)
          levelItemDF <- merge(levelItemDF, ordenDF, by.x = "levelItem", by.y = "valores")
          levelItemDF <- levelItemDF[order(levelItemDF[, "orden"]), ]

          levelItem <- as.character(levelItemDF[, "levelItem"])

          base[, nameItem] <- ordered(varItem, levels = levelItem, labels = labelItem)
        }
        if (verbose) {
          cat("Table of labels against values in lecture for:", nameItem, "\n")
          print(table(addNA(base[, nameItem], ifany = TRUE), addNA(souItemPrev, ifany = TRUE)))
          cat("\n")
        }

      }
    }

    posiNo <- posiEnd + 1
  }

  ################################################################################
  # # final statistics of report
  ################################################################################

  # #
  if (eliminatedVars) {
    variables <- subset(dict$variables, hoja == Block & elimina == "06")[, "id"]
  } else {
    variables <- subset(dict$variables, hoja == Block)[, "id"]
  }

  if (length(variables) == 0) {
    stop("The block ", Block, " doesn't have variables of the questionnaire ", questionnaire,
         " PLEASE check the dictionary or what do you want to do? IDIOT")
  }
  #cat("variables selected to the analysis", variables, "\n")
  if (multiMarkOmiss) {
    pctNA <- rowMeans(base[, variables] == 'NR' | base[, variables] == 'Multimarca' | is.na(base[, variables]))
    cat("\n################################################################################",
        "\nNumber of rows in the file=", nrow(read),
        "\nReports for questionnaire: ", questionnaire,
        "\n  Number of rows with no omission values = ",
        sum(rowSums(base[, variables] == 'NR' | is.na(base[, variables])) == 0),
        "\n  % of complete cases=",
        round(100 * sum(rowSums(base[, variables] == 'NR' | is.na(base[, variables])) == 0) / nrow(base), 2), "%\n",
        "\n  Number of rows with no missing and no multiMarc values=",
        sum(rowSums(base[, variables] == 'NR' | base[, variables] == 'Multimarca' | is.na(base[, variables])) == 0),
        "\n  % of complete cases=",
        round(100 * sum(rowSums(base[, variables] == 'NR' | base[, variables] == 'Multimarca' | is.na(base[, variables])) == 0) / nrow(base), 2), "%\n",
        "\n Number of cases with less than ", 100 * omissionThreshold, "% of missing values= ", sum(pctNA <= omissionThreshold),
        sep = "")
    # # if noOmission is true report the number of rows deleted
    if (noOmission) {
      base <- base[pctNA <= omissionThreshold, c("aplicacion", "noHoja", "cuadernillo", "codSitio", variables)]
      cat("\n  Final number of cases in the data frame ", nrow(base),
          "\n   % of cases retain to the analysis= ", round(100 * nrow(base) / nrow(read), 2), "% \n", sep = "")
    } else {
      cat("All the cases in the fileName are included in the data.frame \n")
    }
  } else {
    pctNA <- rowMeans(is.na(base[, variables]))
    cat("\n################################################################################",
        "\nNumber of rows in the file=", nrow(read),
        "\nReports for questionnaire: ", questionnaire,
        "\n  Number of rows with no missing values=", sum(complete.cases(base[, variables])),
        "\n  % of complete cases=", round(100 * sum(complete.cases(base[, variables])) / nrow(base), 2), "%\n",
        "\n  Number of cases with less than ", 100 * omissionThreshold, "% of missing values= ", sum(pctNA <= omissionThreshold),
        sep = "")
    # # if noOmission is true report the number of rows deleted
    if (noOmission) {
      base <- base[pctNA <= omissionThreshold, c("aplicacion", "noHoja", "cuadernillo", "codSitio", variables)]
      cat("\n  Final number of cases in the data frame ", nrow(base),
          "\n   % of cases retain to the analysis= ", round(100 * nrow(base) / nrow(read), 2), "% \n", sep = "")
    } else {
      cat("\n All the cases in the fileName are included in the data.frame \n")
    }
  }

  base <- droplevels(base)

  return(base)
}

################################################################################
################################################################################
################################################################################

# # Function to generate datBlock (datos por prueba) from datBlockF
# (datos por forma)
datPrueba <- function (prueba, dictionary, datBlockF) {
# # Arg:
# # prueba:  [character] identificacion del numero de prueba
# # dictionary: [character] nombre del archivo que contiene el
# diccionario
# # datBlockF: [character] nombre del archivo que contiene los bloques
# de datos por Forma
  isPrueba <- dictionary$variables[, "codigo_prueba"] == prueba

  quest0 <- unique(subset(dictionary$variables[,"hoja"], isPrueba))
  quest <- paste(substr(quest0,1,1),"A",substr(quest0,2,3),sep="")
  nQuest <- length(quest)

  isQuest <- dictionary$variables[,"hoja"] %in% quest0
  id <- unique(subset(dictionary$variables[,"id"], isPrueba & isQuest))

  if(nQuest == 1) {
    datBlock <- datBlockF[[quest[1]]]
    isId <- names(datBlock) %in% c("aplicacion", "noHoja", "cuadernillo",
                                 "codSitio", id)
    datBlock <- datBlock[, isId]

# #     structure(datBlock, parameters = match.call(),
# #             fechaDescarga = '2014/02/24', prueba = prueba)

   } else  if(nQuest == 2) {
    datBlock <- merge(datBlockF[[quest[1]]], datBlockF[[quest[2]]],
                      by=c("cuadernillo", "aplicacion", "codSitio"), all=TRUE)
    datBlock <- as.data.frame(apply(datBlock, 2, function(x)
                                       ifelse(is.na(x), 'No Presentado', x)) )
    datBlock <- datBlock[,!(names(datBlock)%in%c("noHoja.y"))]
    names(datBlock) <- gsub(".x", "", names(datBlock))
    isId <- names(datBlock) %in% c("aplicacion", "noHoja", "cuadernillo",
                                 "codSitio", id)
    datBlock <- datBlock[, isId]

# #     structure(datBlock, parameters = match.call(),
# #             fechaDescarga = '2014/02/24', prueba = prueba)

  } else {
    # # Dataset by proof
    datBlock0 <- merge(datBlockF[[quest[1]]], datBlockF[[quest[2]]], by="cuadernillo", all=TRUE)
    datBlock0 <- as.data.frame(apply(datBlock0, 2, function(x)
                                  ifelse(is.na(x), 'No Presentado', x)))
    datBlock0 <- datBlock0[,!(colnames(datBlock0)%in%c("aplicacion.y", "noHoja.y", "codSitio.y"))]
    colnames(datBlock0) <- gsub(".x", "", names(datBlock0))
    kk=3
    for(kk in 3:length(quest)) {
      datBlock1 <- merge(datBlock0, datBlockF[[quest[kk]]], by="cuadernillo", all=TRUE)
      datBlock1 <- as.data.frame(apply(datBlock1, 2, function(x)
                                       ifelse(is.na(x), 'No Presentado',
                                              x)) )
      datBlock1 <- datBlock1[, !(names(datBlock1)%in%c("aplicacion.y", "noHoja.y", "codSitio.y"))]
      names(datBlock1) <- gsub(".x", "", names(datBlock1))
      datBlock0 <- datBlock1
    }
    datBlock <- datBlock0
    isId <- names(datBlock) %in% c("aplicacion", "noHoja", "cuadernillo",
                                 "codSitio", id)
    datBlock <- datBlock[, isId]
  }

   structure(datBlock, parameters = match.call(),
            fechaDescarga = '2014/02/24', prueba = prueba)
}


################################################################################
# #  Function to read R.data files with lecture of FA and the dictionary to collapse
################################################################################
ReadDataFAColapsa <- function (dataRead, dict, verbose = TRUE) {
  # # Function to read R.data files with lecture of FA and the dictionary to collapse
  # #
  # # Arg:
  # #   fileName: [character] name of the file of lecture R.Data
  # #   dict: [list] a list with the dictionary of variables (dict$variables), the dictionary
  # #         of categories (dict$categories) and the dictionary of index (dict$index)
  # #   verbose: [logical] (TRUE or FALSE). If TRUE, then tables for each variable are
  # #         printed, shown the final label against the character in lecture
  # # Ret:
  # #  [data.frame] with the variables with the characteristics that
  # #               appear in dictionary list to collapsed

  ################################################################################
  # # required libraries
  ################################################################################
  require(car)

  ################################################################################
  # # general settings of the function
  ################################################################################
  read <- dataRead
  dict$variables <- dict$variables[order(dict$variables[,
                                         "hoja"], dict$variables[, "orden"]), ]
  # # check number of items (variables) in the file are in the dictionary
  itemsBlock <- names(read)[!(names(read)%in%c("aplicacion", "noHoja", "cuadernillo", "codSitio"))]
  isItemPrueba <- dict$variables[, "id"] %in% itemsBlock
  codPrueba <- unique(dict$variables[isItemPrueba, "codigo_prueba"])

  if(length(codPrueba) != 1) {
    stop("All this items = ", paste(itemsBlock, collapse = " "), " must belong to the same 'prueba'")
  }

  itemsDict <- subset(dict$variables[, c("codigo_prueba", "id", "elimina")],
                      codigo_prueba == codPrueba & elimina != "08")
  itemsDict <- unique(itemsDict[, "id"])

  if (!all(itemsDict %in% itemsBlock)) {
    stop("Some items in the dictionary don't
          exist in the datBlock", "\n", paste(itemsDict[!(itemsDict %in%
          itemsBlock)], collapse = " "), " for 'prueba' = ", codPrueba, "\n")
  }

  if (!all(itemsBlock %in% itemsDict)) {
    stop("Some items in the datBlock don't
          exist in the dictionary", "\n", paste(itemsBlock[!(itemsBlock %in%
          itemsDict)], collapse = " "), " for 'prueba' = ", codPrueba,"\n")
  }

  # # check variables in dictionary
  fieldsVar <- c("id", "opRes", "orden", "orient", "codMod", "colapsa", "elimina", "prueba")
  fieldsCat <- c("opRes", "valor", "etiqueta", "tipo")
  if (!all(fieldsVar %in% names(dict$variables))) {
    stop("some fields required in the dictionary of variables are missing: ",
         fieldsVar[!(fieldsVar %in% names(dict$variables))])
  }
  if (!all(fieldsCat %in% names(dict$categories))) {
    stop("some fields required in the dictionary of categories are missing: ",
         fieldsCat[!(fieldsCat %in% names(dict$categories))])
  }

  ################################################################################
  # # creation of the data.frame with the variables collapsed
  ################################################################################

  if (verbose) {
    cat("\n","################################################################################","\n",
        "verbose = TRUE, Table of labels collapsed for 'prueba' = ", codPrueba, "\n",
        "################################################################################","\n")
  }

  for (ii in itemsBlock) {
    variables   <- subset(dict$variables, id == ii)
    variables   <- variables[, fieldsVar]
    categories  <- subset(dict$categories, opRes %in% variables[, "opRes"])
    nameItem    <- variables[, "id"]
    typeItem    <- unique(categories[, c("tipo")])
    collapse    <- subset(dict$collapse, id == nameItem)
    nrowCollap  <- nrow(collapse)
    isCollapse  <- nrowCollap > 0

    isEliminate <- variables[, "elimina"] != "06"

    # # omitted all the lecture of the file if the var is eliminated
    if (isEliminate) {
      if(variables[, "elimina"] == "08"){
        cat("# #",
          nameItem, "was eliminated own due",
          dict$desElim[dict$desElim[, "elimina"] == variables[, "elimina"], "descripcion"],
          "It's omitted from the final data.frame return by the function\n",
          "# #\n\n")
      } else {cat("# #", nameItem, "will be deleted in future steps due to",
          dict$desElim[dict$desElim[, "elimina"] == variables[, "elimina"], "descripcion"],
          "# #\n\n")
      }
    } else {
        if (isCollapse) {
          # # Cambiando de etiqueta a valor las respuetas del item ii
          categoriesOld <- subset(dict$categories, opRes %in% unique(collapse$labAnt))
          recodingAnt   <- paste("'", categoriesOld[, "etiqueta"], "'='",
                               categoriesOld[, "valor"], "'", sep = "")
          recodesAnt   <- paste("'Multimarca'='M'","'NR'='O'", recodingAnt[1], sep = ";")

          for (ll in 2:nrow(categoriesOld)) {
            recodesAnt <- paste(recodesAnt, recodingAnt[ll], sep = ";")
          }

          read[, ii] <- recode(read[,ii],recodesAnt,as.numeric.result = FALSE, as.factor.result = FALSE)
          readItemOld  <- read[, ii]

          # # Cambiando al valor resultante de colapsar el item ii
          recoding <- paste("'", collapse[, "colAnt"], "'='", collapse[, "colNueva"], "'", sep = "")
          recodes <- recoding[1]
          if (nrowCollap > 1) {
            for (ll in 2:nrowCollap) {
              recodes <- paste(recodes, recoding[ll], sep = ";")
            }
          }
          cat(nameItem, "was collapse with", recodes, "\n")
          read[, ii] <- recode(read[, ii], recodes, as.numeric.result = FALSE, as.factor.result = FALSE)

          if (is.na(variables[, "orient"])) {
            cat("For: ", nameItem, " is missing the value of
                orientation, this is assume to be direct\n", sep = "")
            descItem <- FALSE
          } else if (!(variables[, "orient"] %in% c("DIRECTA", "INVERSA"))) {
            cat("Orientation of the variables '", as.character(variables[, "orient"]),
            "' is not between the types availables, this is assume to be direct\n", sep = "")
            descItem <- FALSE
          } else {
            descItem <- substr(variables[, "orient"], 1, 1) == 'I'
          }

        missingValues <- c("O", "M")
        missingLabel  <- c("NR", "Multimarca")
        labelItem <- as.character(categories[order(categories[,
                                                   "valor"], decreasing = descItem), "etiqueta"])
        levelItem <- categories[order(categories[, "valor"], decreasing = descItem), "valor"]
        missingLabel  <- missingLabel[!(missingValues %in% levelItem)]
        missingValues <- missingValues[!(missingValues %in% levelItem)]
        labelItem <- c(missingLabel, labelItem)
        levelItem <- c(missingValues, levelItem)


        read[, ii] <- factor(read[, ii])

        if (typeItem == "Nominal") {
          read[, ii] <-  factor(read[, ii], levels = levelItem, labels = labelItem)
        } else {
          orden <- c(1, 2, categories[, "orden"] + 2)
          valores <- levelItem
          ordenDF <- data.frame(valores, orden, stringsAsFactors = FALSE)
          levelItemDF <- data.frame(levelItem)
          levelItemDF <- merge(levelItemDF, ordenDF, by.x = "levelItem", by.y = "valores")
          levelItemDF <- levelItemDF[order(levelItemDF[, "orden"]), ]

          levelItem <- as.character(levelItemDF[, "levelItem"])

          read[, ii] <- ordered(read[, ii], levels = levelItem, labels = labelItem)
        }
        if (verbose) {
          cat("Table of labels against values in lecture for:", nameItem, "\n")
          print(table(addNA(read[, ii], ifany = TRUE), addNA(readItemOld, ifany = TRUE)))
          cat("\n")
        }
      }
    }
  }

  base <- droplevels(read)

  return(base)
}










