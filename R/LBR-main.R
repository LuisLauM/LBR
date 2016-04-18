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

#' @title Lee las bases de datos de biología reproductiva
#' @description Funcion para leer bases de datos de biología reproductiva desde un directorio
#' especificado.
#' @param filename Archivo conteniendo la base de datos a trabajar.
#' @param ... Argumentos extra, exportados a la función \code{read.csv}.
#' @examples
#' leerLBRDatos(filename)

leerLBRDatos <- function(filename, useAll = TRUE,
                         initialDates = NULL, finalDates = NULL, listMonths = NULL,
                         stringsAsFactors = FALSE, ...){

  # Leer archivos csv
  allData <- read.csv(file = filename, stringsAsFactors = stringsAsFactors)
  colnames(allData) <- tolower(colnames(allData))

  # Mensaje si no existen las columnas de año, mes, día y especie
  if(!all(is.element(c("year", "month", "day", "sp"), colnames(allData))))
    stop("Es necesario que la base de datos contenga columnas de año (year), mes (month),
         día (day) y especie (sp).")

  # Filtrado de filas válidas para información de año, mes, día, especie
  index <- complete.cases(allData[,c("year", "month", "day", "sp")])
  if(sum(index, na.rm = TRUE) < 1)
    stop("No existe ninguna fila válida con información año (year), mes (month), día (day)
         y especie (sp).") else
      allData <- allData[which(index),]

  allData$date <- as.Date(paste(allData$year, allData$month, allData$day, sep = "-"))

  # allData$date <- with(allData, as.Date(paste(year, month, day, "-")))

  # Realizar filtrado por fechas
  allData <- .splitByDate(data = allData, useAll = useAll,
                          iniDates = initialDates, finDates = finalDates, listMonths = listMonths,
                          save = FALSE)

  class(allData) <- c("LBRData", "data.frame")

  return(allData)
}

#' @title Obtiene un objeto con los principales índices reproductivos: IGS, FC, FD
#' @description Funcion para obtener un objeto de clase LBRindex con los principales índices para
#' una especie indicada: Índice Gonadosomático, Factor de Condición, Fracción Desovante
#' @param data Objeto de clase 'LBRData' conteniendo información para el cálculo de
#' IGS (peso gónada, peso eviscerado), FC (pero eviscerado, longitud total)
#' @param ... Argumentos extra, exportados a la función \code{read.csv}.
#' @examples
#' obtenerIndices(filename)

obtenerIndices <- function(data, sp = "anc", ...){

  # Filtro de datos por especie
  index <- is.element(data$sp, sp)
  data <- data[index,]

  # Create outpput file
  allData <- list(data = data, indices = NULL, sp = sp)

  indexNames <- c("igs", "fc")
  allIndex <- list()
  for(i in seq_along(indexNames)){
    # Filtro de variables necesarias según índice
    tempData <- .checkVars(allData[[1]], what = indexNames[i])
    tempData$yearmon <- as.yearmon(tempData$date)

    # Filtro de variables necesarias según índice
    allIndex[i] <- .getIndices(tempData, what = indexNames[i], ...)
  }
  names(allIndex) <- indexNames


  class(allData) <- "LBRindices"

  return(allData)
}
