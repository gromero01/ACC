reportTCT <-  function(x, subPrueba = "Indice") {
  require(DT)
  # # Diagrama de opciones de respuesta
  # x <- outList[[1]]
  x <- data.table(x)
  x[, ind_Sub := as.double(1:nrow(.SD)), by = c("Indice")]
  iniCol <- x[, list(Indice, etiqu, alphaTotal, nItems, pathCMC)]
  cols   <- c("alphaTotal", "raw_alpha", "corItBl", "corItIn")
  x[, (cols) := round(.SD,3), .SDcols = cols]
  x <- suppressWarnings(melt(data.table(x), id = c("ind_Sub", "Indice", "etiqu"), 
            measure = c("id", "raw_alpha", "corItBl", "corItIn")))
  
  # # Representando grado-area en una sola fila
  x <- as.data.frame(x)
  dcastCont <- dcast.data.table(data.table(x), Indice + etiqu ~ ind_Sub + variable, 
                                fun.aggregate = list, value.var = c("value"), drop = "FALSE")
  dcastCont <- merge(unique(iniCol), dcastCont, by = c("Indice", "etiqu")) 
  
  # # Ordenando columnas
  colPos    <- grep("^\\d+_(id|raw_alpha|corItBl|corItIn)", names(dcastCont))
  idItem    <- gsub("(\\d+)_.+", "\\1", names(dcastCont)[colPos])
  colPos    <- split(colPos, f = idItem)
  newOrden  <- sort(as.numeric(names(colPos)), index.return = TRUE)$ix
  colPos    <- colPos[newOrden]

  finTable <- ""
                
  for (ww in names(colPos)) {
    iterInd <- sapply(colPos[[ww]], function(x)  
                      paste0("            '<td>'+ d[", x + 1, "] +'</td>'+\n"))
    auxRow  <- paste0("        '<tr>'+\n            '<td>", ww , "</td>'+\n",
               paste(iterInd, collapse = ""), "        '</tr>'+")
    finTable <- paste(finTable, auxRow, sep = "\n")
  }

  # # Renombrando primeras columnas
  dcastCont <- data.frame(dcastCont)
  names(dcastCont)[1:4] <- c("Codigo", "Indice", "&alpha;", 
                             "No. Items")

  # # Tablas en Html de los items
  htmlTab1 <- datatable(
    cbind(' ' = '', dcastCont), escape = FALSE,
    options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0, 5:(ncol(dcastCont) + 1))),
        list(orderable = FALSE, className = 'details-control', targets = 1)
      ),
    initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}")
    ),  
    callback = JS(
    paste0("table.column(1).nodes().to$().css({cursor: 'pointer'});    
    var format = function(d) {
    return '<table border=\"0\" cellpadding=\"5\" cellspacing=\"0\" style=\"padding-left:95px;\" width=\"100%\" >'+\n",
              "'<tr>' + 
    '<th>Id</th>' + 
    '<th>Item</th>' + 
    '<th>&Delta; &alpha;</th>' + 
    '<th>Correlaci&oacute;n<br>- Bloque</th>' + 
    '<th>Correlaci&oacute;n<br>- &Iacute;ndice</th>' + 
    '<th  colspan=\"4\" rowspan=\"", length(colPos) + 1, 
    "\" align = \"center\">  <img align=\"middle\" style=\"width:660px;height:550px;\" src=\"../'+ 
    d[6] + '\"></th>' + 
  '</tr>' +", finTable, "\n '</table>'};
    table.on('click', 'td.details-control', function() {
    var tr = $(this).closest('tr');
        var row = table.row( tr );
 
        if ( row.child.isShown() ) {
            
            // This row is already open - close it
            row.child.hide();
            tr.removeClass('shown');
        }
        else {
            
            // Open this row
            row.child( format(row.data()) ).show();
            tr.addClass('shown');            
        }
    } );"
  )))
  return(htmlTab1)
}

reporteItem <-  function(x, idPrueba, carNR = c("O", "M")) {
  require(DT)
  require(googleVis)
  # # Diagrama de opciones de respuesta
  # x <- tablaFin; idPrueba <- "f123"
  itObs <- grep("(_mAbility|_prop)", names(x), value = TRUE)
  itObs <- x[, c("item", itObs, "keyItem"), with = FALSE]
  keyIt <- cbind(x[, c("item", "keyItem"), with = FALSE], 'flagKey' = FALSE)
  setnames(keyIt, "keyItem", "categoria")
  itObs <- melt(itObs, id = 1, measure = 2:(ncol(itObs) - 1))
  itObs <- itObs[, list(item, 
                'categoria' = gsub("(\\w)_.+", "\\1", variable), 
                'variable' = gsub("\\w_(.+)", "\\1", variable), value)]
  itObs <- merge(itObs, keyIt, by = c("item", "categoria"), all.x = TRUE)
  itObs[is.na(itObs$flagKey), flagKey := TRUE]

  # # Dividir para cada uno de los items
  itObs <- split(itObs, f = itObs$item)
  itObs <- lapply(itObs, function(xx) {
                  auxX <- data.table(dcast(xx, item + categoria + flagKey ~ variable))
                  auxX <- cbind(auxX, 'Habilidad Promedio' = round(auxX$mAbility, 3))
                  auxX[, mAbility := paste0("Promedio de habilidad: ", round(mAbility, 3))]            
                  setnames(auxX, c("prop", "categoria", "mAbility", "flagKey"), 
                           c("Porcentaje", "Porcentaje.annotation", "Porcentaje.annotationText", 
                             "Porcentaje.certainty"))
                  auxX[, categoria := Porcentaje.annotation]
                  auxX <- subset(auxX, !categoria %in% carNR)
                 return(auxX)})
  
  # # Construccion del grafico con googleVis
  varYtomar <- c("Porcentaje", "Porcentaje.certainty", 
                "Porcentaje.annotation", "Porcentaje.annotationText",
                "Habilidad Promedio")
  varXtomar <- "categoria"
  chartID   <- paste0("grafGPREP", idPrueba)
  exaCombo <- gvisComboChart(itObs[[1]], xvar = varXtomar, chartid = chartID,
                          yvar = varYtomar,
                          options=list(width = 470, height = 310, 
                                       annotations = "{alwaysOutside: true}",
                                       vAxes="[{title:'Porcentaje', format:'#,###%', 
                                              titleTextStyle: {color: 'blue'},
                                              textStyle:{color: 'blue'}, textPosition: 'out'},
                                              {title:'Habilidad',
                                              titleTextStyle: {color: 'grey'},
                                              textStyle:{color: 'grey'}, textPosition: 'out'}]",
                                       vAxis="{gridlines: {color: 'transparent'}}",
                                       legend ='{position: "bottom"}',
                                       series="[{type:'bars', targetAxisIndex:0}, 
                                                {type:'line', targetAxisIndex:1,
                                                color:'grey'}]"))
  # # Formato de % en los toltips
  auxForma <- c('data.addRows(datajson);\n',
                "var formatter = new google.visualization.NumberFormat(\n",
                '{negativeColor: "red", negativeParens:  true,pattern:"#,###.#%"});\n',
                "formatter.format(data, 1);\n")
  exaCombo$html$chart["jsData"] <- gsub('data.addRows\\(datajson\\);',
                                    paste(auxForma, collapse = ""),
                                    exaCombo$html$chart["jsData"])

  # # Agregando parametros que permiten cambiar datos por item
  exaCombo$html$chart["jsData"] <- paste0("var current = 4", exaCombo$html$chart["jsData"])
  exaCombo$html$chart["jsData"] <- gsub('data.addRows\\(datajson\\);',
                                        'data.addRows(datajson[current]);', 
                                        exaCombo$html$chart["jsData"])
  formatDatos <- function(z) {
    z <- data.frame(z)[, gsub(" ", ".", c(varXtomar, varYtomar))]
    gsub("\\],\\[", "],\\\n[", gsub("\\\n", "", gvisFormat(z)$json))
  }
  datChart <- sapply(itObs, function(xx) return(formatDatos(xx)))
  datChart <- paste0("var datajson = [", paste(datChart, collapse = ",\n"), "];")
  exaCombo$html$chart["jsData"] <- gsub("var datajson .+(data\\.addColumn\\('string','categoria'\\))", 
                                        paste0(datChart, "\\1"), exaCombo$html$chart["jsData"])
  # # Final codigo java Script grafico de opciones
  jsonGPREP <- exaCombo$html$chart[c("jsData", "jsDrawChart", "jsDisplayChart")] 
  pathJson  <- paste0("../../Doc/Js/", chartID, ".js")
  if (!file.exists("../../Doc/Js")) {
    dir.create("../../Doc/Js")
  }
  cat(jsonGPREP, file = pathJson)
 cat("<script src=\"Js/", paste0(chartID, ".js"), "\"></script>\n", sep = "")
 cat("<script type=\"text/javascript\" src=\"https://www.google.com/jsapi?callback=displayChart", chartID,"\"></script>\n", sep = "")  # # Numero total de alertas

  # # Numero total de alertas
  x[, nAlertas := sum(FLAGA, FLAGB, FLAGBISE, FLAGCORR,  
          FLAGKEY1, FLAGKEY2, FLAGKEY3, FLAGMEAN, FLAGPROP, FLAGDIFDIS,
          FLAGDIFDIS, FLAGAZAR, na.rm = TRUE), by = "item"]
  if (x[, unique(codMOD)] == "07") {
    initCol <- 8
  }

  if (x[, unique(codMOD)] == "05") {
    initCol <- 7
  }
  
  # # Ordenando columnas
  x <- x[, list(item_blq, item, nAlertas, disc, dif, azar, item_blq, 
                  item, SUBBLOQUE, COMPONENTE, COMPETENCIA, keyItem, 
                  "", TRIED, RIGHT, PCT, "", BISERIAL, 
                  'disc' = paste0(disc, " (", eedisc, ") "),
                  'dif' = paste0(dif, " (", eedif, ") "), dir_OP, dir_ICC,
                  'Mult' = paste0("M: ", round(M_prop * 100, 2), "% (",  round(M_mAbility, 3), ")"), 
                  'Omis' = paste0("O: ", round(O_prop * 100, 2), "% (",  round(O_mAbility, 3), ")")
                  ,FLAGA, FLAGB, FLAGBISE, FLAGCORR, FLAGINFIT, FLAGKEY1, FLAGKEY2, FLAGKEY3, FLAGMEAN,
                  FLAGOUTFIT, FLAGPROP, FLAGDIFDIS, FLAGAZAR, 'azar' = paste0(azar, " (", eeazar, ") "), 
                  'posReporte' = match(x$item, names(itObs)) - 1)]
  
  # # Renombrando primeras columnas
  x <- data.frame(x)
  names(x)[1:7] <- c("Nombre", "C&oacute;digo", "Alertas", 
                     "Discriminaci&oacute;n", "Dificultad", 
                     "Azar", "Alertas")

  # # Tablas en Html de los items
  htmlTab1 <- datatable(
    cbind(' ' = '', x), escape = FALSE,
    options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0, initCol:(ncol(x) + 1))),
        list(orderable = FALSE, className = 'details-control', targets = 1)
      ),
    initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}")
    ),  
    callback = JS(
    paste0("table.column(1).nodes().to$().css({cursor: 'pointer'});    
    var imprimirDato = function(x, label, flag){
      
      if ( x != '' && x != 'null') {
        if (flag == 1) {
          return '<tr>' +
               '<td colspan=\"2\"> <font color=\"red\">'+ label +'</font></td>'+
               '<td colspan=\"1\"> <font color=\"red\">'+ x +'</font></td>' +
               '</tr>'
        } else {
          return '<tr>' +
               '<td colspan=\"2\">'+ label +'</td>' +
               '<td colspan=\"1\">'+ x +'</td>' +
               '</tr>'
        }
      } else {
        return ' '
      }
    };
    var format = function(d) {
      current    = d[40]
      FLAGA      = d[26]
      FLAGB      = d[27]
      FLAGBISE   = d[28]
      FLAGCORR   = d[29]
      FLAGINFIT  = d[30]
      FLAGKEY1   = d[31]
      FLAGKEY2   = d[32]
      FLAGKEY3   = d[33]
      FLAGMEAN   = d[34]
      FLAGOUTFIT = d[35]
      FLAGPROP   = d[36]
      FLAGDIFDIS = d[37]
      FLAGAZAR   = d[38]
      titulos = '<table border=\"0\" cellpadding=\"5\" cellspacing=\"0\" style=\"padding-left:95px;\" width=\"100%\" >'+\n",
              "'<tr>'+
                '<th>Nombre</th>'+
                '<th>C&oacute;digo</th>'+
                '<th>Bloque</th>'+ 
                '<th>Componente</th>'+
                '<th>Competencia</th>'+
                '<th>Clave</th>'+
                '<th>&Delta;&alpha;</th>'+
              '</tr>'+
              '<tr>'+
                '<td>'+ d[8] +'</td>'+
                '<td>'+ d[9] +'</td>'+
                '<td>'+ d[10] +'</td>'+
                '<td>'+ d[11] +'</td>'+
                '<td>'+ d[12] +'</td>'+
                '<td>'+ d[13] +'</td>'+
                '<td>'+ d[14] +'</td>'+
              '</tr>'+
              '<tr>'+
                '<td colspan=\"7\">&nbsp;</td>'+
              '</tr>'+
              '<tr>'+
                '<th colspan=\"2\">Estadisticos del &iacute;tem</th>'+
                '<th colspan=\"1\">Valor</th>'+
                '<th colspan=\"4\">Distribuci&oacute;n opciones de respuestas</th>'+
              '</tr>'
              
    return titulos  + 
    '<tr>'+
      '<td colspan=\"2\">  N </td>'+
      '<td colspan=\"1\">'+ d[15] +'</td>'+
      '<td colspan=\"4\" rowspan=\"7\"> <div id=\"", chartID, "\" style=\"margin-left: 200;\"> </div>' +
      '</td>'+
    '</tr>'+
           imprimirDato(d[16], 'Correctas', 0) +
           imprimirDato(d[17], 'Porcentaje', FLAGMEAN) +           
           imprimirDato(d[18], 'Correlaci&oacute;n biserial excluyendo &iacute;tem', FLAGCORR) +           
           imprimirDato(d[19], 'Correlaci&oacute;n biserial', FLAGBISE) +               
           imprimirDato(d[20], 'Discriminaci&oacute;n', FLAGA) +               
           imprimirDato(d[21], 'Dificultad', FLAGB) +
           imprimirDato(d[39], 'Azar', FLAGAZAR) +                               
    '<tr>'+
      '<td colspan=\"3\">  </td>'+
      '<td colspan=\"2\">'+ d[24] +'</td>'+
      '<td colspan=\"2\">'+ d[25] +'</td>'+
    '</tr>'+
    '<tr>'+
      '<td colspan=\"3\" align=\"center\"> <a href=\"javascript:void(0)\" onclick=\"popup(\\''+ d[22] +'\\')\"><img align=\"middle\" style=\"width:472px;height:300px;\" src=\"../'+ d[22] +'\"></a></td>'+
      '<td colspan=\"4\" align=\"center\"> <a href=\"javascript:void(0)\" onclick=\"popup(\\''+ d[23] +'\\')\"><img align=\"middle\" style=\"width:472px;height:300px;\" src=\"../'+ d[23] +'\"></a></td>'+
    '</tr>'+", "\n'</table>';
    };
    table.on('click', 'td.details-control', function() {
    var tr = $(this).closest('tr');
        var row = table.row( tr );
 
        if ( row.child.isShown() ) {
            
            // This row is already open - close it
            row.child.hide();
            tr.removeClass('shown');
        }
        else {
            
            // Open this row
            row.child( format(row.data()) ).show();
            tr.addClass('shown');
            drawChart", chartID, "();
        }
    } );"
  )))
  htmlTab1
  return(htmlTab1)
}