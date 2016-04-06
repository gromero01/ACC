################################################################################
# # readFilesAI.R
# # R Versions: R version 2.12.0
# #
# # Author(s): Alvaro Uzaheta
# #
# # SABER 5 y 9
# # Description: Function to read files from AnalItem
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
# #   20140211: Review 'no present' items
# #   20140213: Include mixedorder for items with more than 10 levels
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
  fieldsInd <- c("CODIGO", "COMPONENTE", "COMPETENCIA", "ESCALA")
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

  if (!all(dictionary$variables[, "indice"] %in% dictionary$index[, "CODIGO"])
      |
      !all(dictionary$index[, "CODIGO"] %in% dictionary$variables[, "indice"])) {
    varOpRes <- unique(dictionary$variables[, "indice"])
    catOpRes <- unique(dictionary$index[, "CODIGO"])
    if (!all(varOpRes %in% catOpRes)) {
      cat("# # Warning:
          Some values for indice in the variables dictionary don't
          exist in the index dictionary:\n",
          CheckMiss(varOpRes, catOpRes), "\n")
    }
    if (!all(catOpRes %in% varOpRes)) {
      cat("# # Warning:
          Some values for CODIGO in the index dictionary don't
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

ReadDataAI <- function (folderName, dict,
                      conFile = "pba\\w+\\.con$", datFile = "\\.dat$",
                      multiMarkOmiss = TRUE, verbose = TRUE,
                      eliminatedVars = TRUE) {
  # # Function to read data files with lecture of B sheets from the input
  # #
  # # Arg:
  # #   folderName: [character] name of the folder with the files
  # #               download from AnalItem
  # #   dict: [list] an object result from the function ReadDict
  # #   conFile: [character] the name or a regular expression that allow
  # #             identify one file .con with the names of the items
  # #   datFile: [character] the name or a regular expression that allow
  # #             identify the file .dat with the data from AnalItem
  # #   multiMarkOmiss: [logical] (TRUE or FALSE). Should multi mark and
  # #                   omission values include as levels of the variables?
  # #   verbose: [logical] (TRUE or FALSE). If TRUE, then tables for
  # #             each variable are printed, shown the final label
  # #             against the character in lecture
  # #   eliminatedVars: [logical] (TRUE or FALSE). IF TRUE eliminated
  # #                   Vars in the dictionary are read in the return
  # #                   data.frame, if FALSE eliminated vars are omitted
  # #                   in the return data.frame
  # #
  # # Ret:
  # #  [data.frame] that has the variables with the characteristics that
  # #               apper in dict list and the variables for
  # #               identification of the number of sheet

  ################################################################################
  # # required libraries
  ################################################################################
  require(car)  # # to collapse categories
  require(LaF)  # # to read data.set
  require(gtools)  # # for mixedorder

  ################################################################################
  # # validate parameters
  ################################################################################
  if (!exists(as.character(substitute(dict)))) {
    stop("Object: ", dict, " doesn't exist")
  }

  if (class(dict) != "RD") {
    stop("Dict parameter is expected to be a output of the ReadDict function")
  }


  if (!file.exists(folderName)) {
    stop("Folder: ", folderName, " doesn't exist")
  }

  if (!all(is.logical(c(multiMarkOmiss, verbose, eliminatedVars)))) {
    stop("Any or some of the next parameter are not of type logical:
         multiMarkOmiss verbose eliminatedVars")
  }

  if (!all(is.character(c(conFile, datFile)))) {
    stop("Arguments conFile and datFile expected to be character type")
  }

  if (length(c(conFile, datFile)) > 2) {
    stop("More than one conFile or datFile names was specified")
  }

# #   if (omissionThreshold <0 | omissionThreshold > 1) {
# #     stop("The value for omissionThreshold is not between 0 and 1")
# #   }
# #   if (!(questionnaire %in% unique(dict$variables[, "questionnaire"]))) {
# #     stop("The value for questionnaire doesn't exist in the dictionary of variables")
# #   }

  ################################################################################
  # # .com file with the items
  ################################################################################
  # # report of the proof and version that is going to be read
# #   cat(folderName)
  estructura <- ".*/(S[AB]20\\d+)_(\\d+_\\d+_\\d+)_(\\d+_\\d+_\\d+)/.*"
  if (length(grep(estructura, folderName)) == 0) {
    warning("The folderName parameter doesn't have the expected structure")
  } else {
    prueba  <- gsub(estructura, "\\1", folderName)
    dia     <- gsub(estructura, "\\2", folderName)
    dia     <- as.Date(dia, "%Y_%m_%d")

    cat("#########################################################################",
        "\n# # Reading: ", prueba, #"\n# # Version: ", version,
        "\n# # Date   : ", as.character(dia),
        "\n#########################################################################\n")
  }

  # # files that are inside folderName also in subdirectories
  filesInFolder <- list.files(folderName, recursive = TRUE,
                              pattern = "con|dat$", ignore.case = TRUE)

  # #
  fileCon <- grep(conFile, filesInFolder, value = TRUE, ignore.case = TRUE)

  if (length(fileCon) == 1) {
    inFileCon <- paste(folderName, fileCon, sep = '/')
    conInfo   <- read.table(file = inFileCon, sep = '\t',
                            colClasses = 'character')
  } else if (length(fileCon) == 0) {
    stop("Can't find any file with the form: ", conFile,
         "\n in the directory: ", folderName, "\n or in any subdirectory inside")
  } else {
    stop("Find more than one file with conFile characteristic:\n",
         paste(fileCon, collapse = "\n"),
         "\n please specify a correct folderName or conFile to avoid multiple files")
  }

  nSItem <- grep("&END", conInfo[, 'V1']) + 1
  nEItem <- grep("END NAMES", conInfo[, 'V1']) - 1

  if (length(nSItem) == 0 | length(nEItem) == 0) {
    stop("Is imposible determined the posicions where the name of the
         items start of finished in the file:\n", inFileCon,
         "\nto start search for '&END' to finish for 'END NAMES'")
  }

  if (length(nSItem) > 1) {
     warning("Multiple posicions has '&END' in the file:\n",
             inFileCon, "\n only the first was taken")
     nSItem <- nSItem[1]
   }

  if (length(nEItem) > 1) {
     warning("Multiple posicions has 'END NAMES' in the file:\n",
             inFileCon, "\n only the first was taken")
     nEItem <- nEItem[1]
   }

  items <- substr(conInfo[nSItem:nEItem, "V1"], 1, 7)

  if (all(order(items) != seq(length(items)))) {
    stop("Disorder of the items in the .com file")
  }

  posNI <- grep("NI =", conInfo[, 'V1'])
  nItems <- as.numeric(gsub("NI = (\\d+)", "\\1", conInfo[posNI, 'V1']))

  cat("Reading from file:\n", inFileCon, "\n  a total of =",
      length(items), "codes of items are in the .con file \n",
      ".con file reports ", nItems, "items\n")

  if (length(items) != nItems) {
    stop("the number of codes of items is different from the number of
         items reports in the .con file")
  }

  ################################################################################
  # # .dat file, the structure of the id is given here
  ################################################################################
  fileDat <- grep(datFile, filesInFolder, value = TRUE,
                  ignore.case = TRUE)

  if (length(fileDat) == 1) {
    inFileCon <- paste(folderName, fileDat, sep = '/')
  } else if (length(fileDat) == 0) {
    stop("Can't find any file with the form: ", datFile,
         "\n in the directory: ", folderName, "\n or in any subdirectory inside")
  } else {
    stop("Find more than one file with conFile characteristic:\n",
         paste(fileDat, collapse = "\n"),
         "\n please specify a correct folderName or conFile to avoid multiple files")
  }

  cat("Reading file:", fileDat, "\n")

  #long <- scan(inFileCon, n = 2, what = 'character')
  anchos <- c(8, 2, 1, 1, 6, 5, 8, nItems)
  nombres <- c("snp", "letraCuad", "grado", "tipoApli", "sejoId",
               "codMpio", "consLect", "string")
  estructura <- data.frame(posiciones = anchos, variable = nombres)
  anchos <- c(8, 2, 1, 1, 6, 5, 8, rep(1, nItems))
  nombres <- c("snp", "letraCuad", "grado", "tipoApli", "sejoId",
               "codMpio", "consLect", items)
  read <-  laf_open_fwf(filename = inFileCon, column_widths = anchos,
                        column_types = rep("string", length(nombres)),
                        column_names = nombres)
  read        <- read[ , ]
  names(read) <- nombres
  cat("#########################################################################",
  "\n# # The file is reading with the follow structure: \n")
  print(estructura)
  cat("\n#########################################################################\n")


  cat("Number of rows read:", dim(read)[1], "\n")

# #   read[, c("string")]          <- gsub(" ", "X", read[, c("string")])

  # # the number of rows deleted by complete omision structure
  omission <-  apply(read[, items], 1,
                     function (x) nchar(gsub("O|\\s", "",
                                             paste(x, collapse = ''))))

  cat("Rows with only omission character:", sum(omission == 0),"\n")
  read     <- read[omission != 0, ]
  cat("Rows after delete only omission character:", dim(read)[1],"\n")

  # # comparision of the number if items in dictionary and in the input file
  areItems <- dict$variables[, 'id'] %in% items
  itemsBlock   <- dict$variables[areItems, "id"]

  pruebas <- dict$variables[areItems, c('id', 'prueba')]

  cat("Number of items in dictionary: ", length(itemsBlock),
      ". Number of character in string: ", nItems, "\n", sep = "")
  if (length(itemsBlock) != nItems) {
     stop("\n\nERROR: Number of items in the dictionary is not equal
          to the number of items in the .com file")
  }

  # # check that not all the items were eliminated
  if (eliminatedVars) {
    variables <- subset(dict$variables, id %in%  itemsBlock
                        & elimina == "06")[, "id"]

  if (length(variables) == 0) {
    stop("The dictionary doesn't have variables that the .com file
         report which have not been eliminated  \n",
          " PLEASE check the dictionary")
  }

  }

  itemPrueba <- aggregate(pruebas[, 'id'],
                          list(prueba = pruebas[, 'prueba']), length)
  names(itemPrueba)[2] <- 'nItems'

  cat("Number of items by prueba:\n")
  print(itemPrueba)
  # # check that the variables needed for decoding exist
  fieldsVar <- c("id", "opRes", "orden", "orient", "codMod", "colapsa",
                 "elimina", "prueba")
  fieldsCat <- c("opRes", "valor", "etiqueta", "tipo", "orden")
  if (!all(fieldsVar %in% names(dict$variables))) {
    stop("some fields required in the dicitonary of variables are missing: ",
          CheckMiss(fieldsVar, names(dict$variables)))
  }
  if (!all(fieldsCat %in% names(dict$categories))) {
    stop("some fields required in the dicitonary of categories are missing: ",
          CheckMiss(fieldsCat, names(dict$categories)))
  }

  ################################################################################
  # # creation of the data.frame with the variables report in the dictionary
  ################################################################################

  if (verbose) {
    cat("\n verbose = TRUE, labels againts string in \n")
  }

  noUse <- names(read) %in% items
  base <- read[, !noUse]

  # # recoded of variables

  for (nameItem in items) {
    toExtract   <- dict$variables[, "id"] == nameItem
    variables   <- dict$variables[toExtract, fieldsVar]
    categories  <- subset(dict$categories, opRes %in% variables[, "opRes"])
    typeItem    <- unique(categories[, c("tipo")])

    if (!is.null(attr(dict, "call")$collapse)) {
      collapse    <- subset(dict$collapse, id == nameItem)
      nrowCollap  <- nrow(collapse)
      isCollapse  <- nrowCollap > 0
    }else {
      isCollapse <- FALSE
    }

    isEliminate <- variables[, "elimina"] != "06"
    #isQuestionnaire <- variables[, "prueba"] == questionnaire

    # # omitted all the lecture of the file if the var is eliminated
    if (eliminatedVars & isEliminate) {

      if (!is.null(attr(dict, "call")$desElim)) {
        isDesElim <- dict$desElim[, "elimina"] == variables[, "elimina"]
        cat("# #",
            nameItem, "was eliminated own due",
            dict$desElim[isDesElim, "descripcion"],
            "It's omitted from the final data.frame return by the function\n",
            "# #\n\n")
      } else {
        cat("# #",
            nameItem,
            "It's omitted from the final data.frame return by the function,
            but we don't why\n",
            "# #\n\n")
      }
    } else {
      if (any(is.na(variables[, "orient"]))) {
        cat("For: ", nameItem,
            " is missing the value of orientation, this is assume to be direct\n",
            sep = "")
        descItem <- FALSE
      } else if (!any(variables[, "orient"] %in% c("DIRECTA", "INVERSA"))) {
        cat("Orientation of the variables '", as.character(variables[, "orient"]),
          "' is not between the types availables, this is assume to be direct\n",
          sep = "")
        descItem <- FALSE
        descItem <- substr(variables[, "orient"], 1, 1) == 'I'
        if (descItem) {
          cat("However, the direction of the item is change\n")
        } 
      } else {
        descItem <- substr(variables[, "orient"], 1, 1) == 'I'
      }

      # # check if the categories have an unique type
      if (length(typeItem) > 1) {
        stop("Type of variable ", nameItem, " is not unique")
      }

      if (!(typeItem %in% c("Continua", "Nominal", "Ordinal", "Dicotomica"))) {
        stop("The type of the item '", typeItem,
             "' is not between the types availables:\n",
             paste(c("Continua", "Nominal", "Ordinal", "Dicotomica"),
                   collapse = "\n"))
      }

      # # statements by type of the variable
      if (typeItem == "Continua") {
        stop("typeItem == 'Continua' is not implemented
             for AnalItem files")
      } else {
        # #         souItemPrev <- substr(read[, "string"], posiNo, posiEnd)

        if (isCollapse) {
          recoding <- paste("'", collapse[, "colAnt"],
                            "'='", collapse[, "colNueva"], "'",
                            sep = "")
          recodes <- recoding[1]
          if (nrowCollap > 1) {
            for (ll in 2:nrowCollap) {
              recodes <- paste(recodes, recoding[ll], sep = ";")
            }
          }
          cat(nameItem, "was collapse with", recodes, "\n")
          souItem <- car::recode(read[, nameItem], recodes,
                                 as.numeric.result = FALSE,
                                 as.factor.result = FALSE)
        } else {
          souItem <- read[, nameItem]
        }

        missingValues <- c("", "O", "M")
        missingLabel  <- c("No Presentado", "NR", "Multimarca")

# #         missingValues <- c("", " ", "O", "M")
# #         missingLabel  <- c("No Presentado", "No Presentado", "NR", "Multimarca")


        orden <- mixedorder(categories[, "orden"])

        if (descItem) {
          orden <- orden[seq(length(orden), 1)]
        }

        labelItem <- categories[orden, "etiqueta"]
        levelItem <- categories[orden, "valor"]

        missingLabel  <- missingLabel[!(missingValues %in% levelItem)]
        missingValues <- missingValues[!(missingValues %in% levelItem)]

        #########################################################################
        # # inclusion of No Aplica possibility for conditional
        # # 121129: conditional is not adress in this function
        #########################################################################
        # #         hasNA <- any(levelItem %in% 'N')
        # #         if (hasNA) {
        # #           whichNA <- which(levelItem %in% 'N')
        # #           labelItem <- c(labelItem[whichNA], labelItem[-whichNA])
        # #           levelItem <- c(levelItem[whichNA], levelItem[-whichNA])
        # #         }

        #########################################################################
        # # decoded
        #########################################################################
        # # if multiMarkOmiss is false the values in missing values are
        # # dropped
        if (multiMarkOmiss) {
          varItem   <- factor(souItem)
          labelItem <- c(missingLabel, labelItem)
          levelItem <- c(missingValues, levelItem)
        } else {
          varItem   <- factor(souItem, exclude = missingValues)
        }

        # # if typeItem Ordinal imposed the order of categories,
        # # the Dicotomica type is considered as Ordinal
        if (typeItem == "Nominal") {
          base[, nameItem] <-  factor(varItem, levels = levelItem,
                                      labels = labelItem)
        } else {
          base[, nameItem] <- ordered(varItem, levels = levelItem,
                                      labels = labelItem)
        }

        if (verbose) {
          cat("Table of labels against values in lecture for:", nameItem, "\n")
          print(table(addNA(base[, nameItem], ifany = TRUE),
                      addNA(read[, nameItem], ifany = TRUE)))
          cat("\n")
        }

      }
    }

  }

  ################################################################################
  # # final statistics of report
  ################################################################################
  isDef <- names(base) %in% items
  finalItems <- names(base)[isDef]

  if (multiMarkOmiss) {
    pctNA <- rowMeans(base[, finalItems] == 'NR' |
                      base[, finalItems] == 'Multimarca' |
                      base[, finalItems] == 'No Presentado')

    cat("\n###################################################################",
        "#############",
      "\nNumber of rows in the file=", nrow(read),
      "\n  Number of rows with no omission values = ",
      sum(rowSums(base[, finalItems] == 'NR') == 0),
      "\n  % of complete cases=",
      round(100 * sum(rowSums(base[, finalItems] == 'NR') == 0) / nrow(base), 2),
      "%\n",
      "\n  Number of rows with no missing and no multiMarc values=",
      sum(rowSums(base[, finalItems] == 'NR' |
                  base[, finalItems] == 'Multimarca'|
                  base[, finalItems] == 'No aplica') == 0),
      "\n  % of complete cases=",
      round(100 * sum(rowSums(base[, finalItems] == 'NR' |
                              base[, finalItems] == 'Multimarca'|
                              base[, finalItems] == 'No aplica') == 0) /
                                nrow(base), 2), "%\n",
      sep = "")
  } else {
    pctNA <- rowMeans(is.na(base[, finalItems]))
    cat("\n###################################################################",
        "#############",
      "\nNumber of rows in the file=", nrow(read),
      "\n  Number of rows with no missing values=",
      sum(complete.cases(base[, finalItems])),
      "\n  % of complete cases=",
      round(100 * sum(complete.cases(base[, finalItems])) / nrow(base), 2), "%\n",
      sep = "")
  }
    # # elimina las categorias que no existen
    base[, ] <- lapply(base, function(x) x[, drop = TRUE])

    structure(base, parameters = match.call(),
              fechaDescarga = dia, prueba = prueba)
}


