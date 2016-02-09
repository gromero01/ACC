################################################################################
# # log.R
# # R Versions: 3.0.2
# #
# # Author(s): Alvaro Uzaheta
# #
# # SB 359
# # Description: Update log with information from the current run
# #
# # Inputs: some objects from the environment
# #
# # Outputs: Update the log file
# #
# # File history:
# #   20140213: Creation
################################################################################

################################################################################
# # create function
################################################################################

RunLog <- function () {
  # # Update log file with information from the current run
  # #
  # # Arg:
  # #  ..
  # #
  # # Ret:
  # #  NULL
  
  if (!file.exists(logFile)) {
    file.create(logFile) 
  } 
  
  fecha <- unique(sapply(datBlock, function(x) 
                           as.character(attr(x, "fechaDescarga"))))
# 
  versionComment2 <- gsub("\n", "\n# #   ", versionComment)
    

  cat("# # Fecha: ", format(Sys.time(), "%Y %b %d %a %H:%M:%S"), "\n",
      "# # Versión: ", versionOutput, "\n",
      "# # Comentario: ", versionComment2, "\n",
      "# # Aplicación: ", unique(sapply(datBlock, attr, "prueba")), "\n",
      "# # Fecha de descarga: ", fecha, "\n",
      "# # Diccionario usado: ", attr(dictionaryList, "features")[1], "\n",
      "# # Hoja de excel dicc. variables: ", 
        attr(dictionaryList, "features")[2], 
      "\n\n",
      file = logFile, append = TRUE, sep = "" 
     )

  invisible(NULL)
}
