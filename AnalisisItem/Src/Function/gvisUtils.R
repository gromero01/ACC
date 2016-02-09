gvisFormat <- function(data){
  #restore.point("gvisFormat")
  ## Create a list where the Google DataTable type of all variables will be stored
  
  ## Convert data.frame to list
  x <- as.list(data)
  varNames <- names(x)
  
  varTypes <- sapply(varNames,
                     function(.x){
                       switch(class(x[[.x]])[1],"integer"="number",
                              "numeric"="number",
                              "character"="string",
                              "factor"="string",
                              "ordered"="string",
                              "logical"="boolean",
                              "Date"="date",
                              "POSIXct"="datetime",
                              "POSIXlt"="datetime")
                     }
  )
  
  ## factor to character
  x.df <- as.data.frame(
    lapply(x,
           function(a){
             if (is.factor(a)) as.character(a) else a
           }
    ),
    stringsAsFactors=FALSE
  )
  
  ## The function is specified above
  json <- toJSONarray(x.df)
  output <- list(
    data.type = unlist(varTypes),
    json = json
  )
  
  output$json <-fixBackslash(output$json)
  
  return(output)
}


toJSONarray <- function(dtf){
  ## Thanks to Sebastian Kranz for this function
  ## Thanks also to Wei Luo: http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis
  
  #restore.point("toJSONarray")
  clnms <- colnames(dtf)
  
  # Transforms a vector into a vector of JSON strings that
  # can be pasted together afterwards
  toJSONvec <- function(vec) {
    #restore.point("name.value")
    na.row <- is.na(vec)
    if(is(vec,'integer')){
      ret <- vec
    } else if (is(vec,'numeric')) {
      # Round to 10 points after the decimal as before
      ret <- as.character(signif(vec,digits=10))
    } else if (is(vec,'logical')) {
      ret <- tolower(as.character(vec))
    } else if (is(vec,'Date')) {
      y <- format(vec,"%Y")
      m <- as.numeric(format(vec,"%m")) -1
      d <- as.numeric(format(vec,"%d"))
      ret <- paste("new Date(",y,",",m,",",d,")",sep="")
    } else if (is(vec,'POSIXct') | is(vec,'POSIXlt')) {
      y <- format(vec,"%Y")
      m <- as.numeric(format(vec,"%m")) -1
      d <- as.numeric(format(vec,"%d"))
      H <- as.numeric(format(vec,"%H"))
      M <- as.numeric(format(vec,"%M"))
      S <- as.numeric(format(vec,"%S"))
      ret <- paste("new Date(",y,",",m,",",d,",",H,",",M,",",S,")",sep="")
    } else {
      quote <- '"';
      vec <- gsub('"', '\\\\"', vec)
      ret <- paste(quote, vec, quote, sep='')
    }
    ret[na.row] <- "null"
    ret
  }
  # Transform columns depending on data type and store in a list
  objs <- lapply(dtf,toJSONvec)
  # Remove names just for the case that one column name was sep or collapse
  names(objs) <- NULL
  # Paste columns together
  str <- do.call(paste,c(objs,list(sep=",\n")))
  
  # Add [ ] and paste rows together
  res <- paste('[\n ', paste("[\n",str,"\n]",collapse=',\n'), ' \n]',sep="")
  return(res)
}

fixBackslash <- function(x){
  x <-  gsub("\\\\\\\\", "\\\\", x)
  return(x)
}
