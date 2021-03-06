# path package: LBR ---------------

#' LBR
#'
#' \tabular{ll}{ Package: \tab LBR\cr Type: \tab Package\cr Version: \tab
#' 1.0\cr Date: \tab 2016-05-01\cr License: \tab GPL-2\cr }
#'
#' @name LBR-package
#' @docType package
#' @keyword reproductive
#' @examples
#'

#' @title Lee las bases de datos de biolog�a reproductiva
#' @description Funcion para leer bases de datos de biolog�a reproductiva desde un directorio
#' especificado.
#' @param filename Archivo conteniendo la base de datos a trabajar.
#' @param ... Argumentos extra, exportados a la funci�n \code{read.csv}.
#' @examples
#' leerLBRDatos(filename)

leerLBRDatos <- function(filename, useAll = TRUE,
                         initialDates = NULL, finalDates = NULL, listMonths = NULL,
                         stringsAsFactors = FALSE, ...){

  # Leer archivos csv
  allData <- read.csv(file = filename, stringsAsFactors = stringsAsFactors)
  colnames(allData) <- tolower(colnames(allData))

  # Mensaje si no existen las columnas de a�o, mes, d�a y especie
  if(!all(is.element(c("year", "month", "day", "sp"), colnames(allData))))
    stop("Es necesario que la base de datos contenga columnas de a�oo (year), mes (month),
         d�a (day) y especie (sp).")

  # Filtrado de filas v�lidas para informaci�n de a�o, mes, d�a, especie
  index <- complete.cases(allData[,c("year", "month", "day", "sp")])
  if(sum(index, na.rm = TRUE) < 1){
    stop("No existe ninguna fila v�lida con informaci�n a�o (year), mes (month), d�a (day)
         y especie (sp).")
  }else{
    allData <- allData[which(index),]
  }

  allData$date <- as.Date(paste(allData$year, allData$month, allData$day, sep = "-"))

  # allData$date <- with(allData, as.Date(paste(year, month, day, "-")))

  # Realizar filtrado por fechas
  allData <- .splitByDate(data = allData, useAll = useAll,
                          iniDates = initialDates, finDates = finalDates, listMonths = listMonths,
                          save = FALSE)

  class(allData) <- c("LBRData", "data.frame")

  return(allData)
}

#' @title Obtiene un objeto con los principales �ndices reproductivos: IGS, FC, FD
#' @description Funci�n para obtener un objeto de clase LBRindex con los principales �ndices para
#' una especie indicada: �ndice Gonadosom�tico, Factor de Condici�n, Fracci�n Desovante
#' @param data Objeto de clase 'LBRData' conteniendo informaci�n para el c�lculo de
#' IGS (peso g�nada, peso eviscerado), FC (pero eviscerado, longitud total)
#' @param ... Argumentos extra, exportados a la funci�n \code{read.csv}.
#' @examples
#' myData <- leerLBRDatos(filename)
#' obtenerIndices(myData)

obtenerIndices <- function(data, sp = "anc", useIGS = TRUE, ...){

  # Filtro de datos por especie
  index <- is.element(data$sp, sp)
  data <- data[index,]

  # Create outpput file
  allData <- list(data = data, indices = NULL, sp = sp)

  indexNames <- c("igs", "fc", "ar")
  allIndex <- list()
  for(i in seq_along(indexNames)){

    # Filtro de variables necesarias seg�n �ndice
    tempData <- .checkVars(allData[[1]], what = indexNames[i])
    tempData$yearmon <- zoo::as.yearmon(tempData$date)

    # Efectuar filtros de Solo adultos, Solo hembras, a partir de una talla de maduraci�n
    index <- (tempData$sex == 0 & tempData$mad != species$virginal &
                tempData$length >= species$talla_maduracion)
    tempData <- tempData[index,]

    # Filtro de variables necesarias seg�n �ndice
    tempIndex <- .getIndices(tempData, what = indexNames[i], sp, ...)

    if(indexNames[i] == "igs" && isTRUE(useIGS)){
      tempIndex <- with(tempData, data.frame(time = date, igs = igs))
    }

    if(is.data.frame(tempIndex) && is.element("time", colnames(tempIndex)))
      tempIndex <- tempIndex[order(tempIndex$time),]

    allIndex[[i]] <- tempIndex
  }
  names(allIndex) <- indexNames

  allData$indices <- allIndex


  class(allData) <- "LBRindices"

  return(allData)
}
