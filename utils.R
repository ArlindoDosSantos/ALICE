options(encoding = "UTF-8")

# fct permettant de lire un couple cle-valeur dans un fichier property
# inspiré de properties::read.properties mais modifié pour nos besoins 
fctReadProperty <- function(file, fields = NULL, encoding = "UTF-8") 
{
  if (is.character(file)) {
    file <- file(file, "r", encoding = encoding)
    on.exit(close(file))
  }
  
  if (!inherits(file, "connection")) 
    stop("'file' must be a character string or connection")
  
  lines <- readLines(file)
  commentedLines <- grepl("^#.*$", lines)
  lines <- lines[!commentedLines]
  line_is_not_empty <- !grepl("^[[:space:]]*$", lines)
  lines <- lines[line_is_not_empty]
  line_has_tag <- grepl("^[^[:blank:]][^=:]*:=", lines)
  
  tuples <- c()
  for (i in length(lines):1 )
  {
    if (!line_has_tag[i])
      lines[i - 1] <- paste0(lines[i - 1], ";", lines[i]) # le tuple se poursuit sur plusieurs lignes
    else
      tuples <- c(lines[i], tuples)
  }
  
  keys <- gsub("^([^:=]+):=.*$", "\\1", tuples)
  values <- gsub("^[^:=]+:=(.*)$", "\\1", tuples)
  names(values) <- keys
  out <- as.list(values)
  
  out <- if (!is.null(fields)) 
    out[fields]
  else out
  return(out)
}

trace <- function(traceLevel, message, niveau = "INFO")
{
  niveaux <- c("DEBUG", "INFO", "WARNING", "ERROR", "FATAL")
  indice <- which(niveaux == niveau)
  indiceTraceLevel <- which(niveaux == traceLevel)
  if (indice >= indiceTraceLevel)
      cat(paste0("\n", substr(x = Sys.time(), start = 1, stop = 19), " [", niveau, "] ", message))
}