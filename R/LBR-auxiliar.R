# Funci?n getDates
# Toma, de manera interactiva, valores de fechas (iniciales y finales) y genera una lista con ellos
.getDates <- function(dates)
{
  dates <- as.character(dates)
  iniDates <- min(as.Date(dates), na.rm = TRUE)
  finDates <- max(as.Date(dates), na.rm = TRUE)

  newIni <- newFin <- NULL
  for(i in seq(as.numeric(readline("¿Cuántas secciones desea tomar?: "))))
  {
    while(is.null(newIni) | is.null(newFin) |
          if(!is.null(newIni) & !is.null(newFin))
          {
            as.Date(newIni, format = "%d/%m/%Y") < iniDates[1] |
              as.Date(newFin, format = "%d/%m/%Y") > finDates[1] |
              as.Date(newIni, format = "%d/%m/%Y") > as.Date(newFin, format = "%d/%m/%Y")
          } else FALSE)
    {
      newIni <- readline(paste("Fecha inicio, sección", i, "(DD/MM/AAAA): "))
      newFin <- readline(paste("Fecha final,  sección", i, "(DD/MM/AAAA): "))
    }

    iniDates <- c(as.character(iniDates), as.character(as.Date(newIni, format = "%d/%m/%Y")))
    finDates <- c(as.character(finDates), as.character(as.Date(newFin, format = "%d/%m/%Y")))
    newIni <- newFin <- NULL
  }

  return(list(iniDates = iniDates[-1], finDates = finDates[-1]))
}

# Get IGS index
.getIndex_IGS <- function(data, ...){

  igsIndex <- with(data, wgonad/wevisc*100)

  return(igsIndex)
}

# Get FC index
.getIndex_FC <- function(data, ....){
  fcIndex <- with(data, wevisc/length^3)

  return(fcIndex)
}
