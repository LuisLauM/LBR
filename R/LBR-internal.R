# Función para filtrar por fechas
.splitByDate <- function(data, useAll, iniDates, finDates, listMonths,
                         save = NULL, filename = NULL){
  data <- data[!is.na(data$year) & !is.na(data$month) & !is.na(data$day),]

  # Crear columna de fechas
  data$date <- as.Date(with(data, paste(year, month, day, sep = "-")))

  # Extraer filas con meses indicados en "listMonths"
  if(!is.null(listMonths))
  {
    newData <- NULL
    for(i in listMonths)
      newData <- rbind(newData, data[data$month == i,])

    data <- newData[order(newData$date),]
  }

  # Generar objeto de fechas iniciales y finales
  if(!is.null(iniDates) & !is.null(finDates)){
    dates <- list(iniDates = as.Date(iniDates, format = "%d/%m/%Y"),
                  finDates = as.Date(finDates, format = "%d/%m/%Y"))
  }else if(!isTRUE(useAll) & (is.null(iniDates) | is.null(finDates))){
    cat(paste("Rango de fechas: Desde ", min(data$date), " hasta ", max(data$date), "\n", sep = ""))
    dates <- .getDates(data$date)
  }else{
    dates <- list(iniDates = min(data$date), finDates = max(data$date))
  }

  data <- data.frame(index = seq(nrow(data)), data)

  # Obtener filas según las fechas
  dateIndex <- NULL
  for(i in seq_along(dates$iniDates)){
    tempIndex <- data$date >= dates$iniDates[i] & data$date <= dates$finDates[i]
    dateIndex <- cbind(dateIndex, tempIndex)
  }

  # Realizar split
  dateIndex <- apply(dateIndex, 1, any)
  newData <- data[dateIndex,]

  # Ordenar por fecha
  newData <- newData[order(newData$date),]

  return(newData)
}

# Función que verifica la presencia de las variables requeridas para el cálculo de índices
.checkVars <- function(data, what){

  what <- if(what == "all") c("igs", "fc")

  for(i in what){

    tempDepends <- .indexDepends$depends[.indexDepends$index == i]
    tempDepends <- unlist(strsplit(x = tempDepends, split = ",", perl = TRUE))
    tempDepends <- gsub(pattern = " ", replacement = "", x = tempDepends, perl = TRUE)

    # Mensaje si no existen las columnas de las variables necesarias
    index <- is.element(tempDepends, colnames(data))
    if(!all(index)){
      index <- match(tempDepends, .varsNames$varAbb)
      varsDepends <- paste0(.varsNames$varName[index], " (", .varsNames$varAbb[index], ")")

      myMessage <- paste0("Es necesario que la base de datos contenga columnas de: ",
                          paste(varsDepends, collapse = ", "),  ".")

      stop(myMessage)
    }

    # Filtrado de filas válidas para variables requeridas por el índice
    index <- complete.cases(data[,c("wgonad", "wevisc")])
    if(sum(index, na.rm = TRUE) < 1){
      index <- match(tempDepends, .varsNames$varAbb)
      tempDepends <- paste0(.varsNames$varName[index], " (", .varsNames$varAbb[index], ")")

      myMessage <- paste0("No existe ninguna fila válida con información de ",
                          paste(tempDepends, collapse = ", "),  ".")

      stop(myMessage)
    }else{
      data <- data[which(index),]
    }
  }

  return(data)
}

# Función de obtención de índices
.getIndices <- function(data, what, ...){
  indexFUN <- match.fun(paste0(".getIndex_", toupper(what)))

  allData <- indexFUN(data = data, ...)

  return(allData)
}
