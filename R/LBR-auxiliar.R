# Funci?n getDates
# Toma, de manera interactiva, valores de fechas (iniciales y finales) y genera una lista con ellos
.getDates <- function(dates)
{
  dates <- as.character(dates)
  iniDates <- min(as.Date(dates), na.rm = TRUE)
  finDates <- max(as.Date(dates), na.rm = TRUE)

  newIni <- newFin <- NULL
  for(i in seq(as.numeric(readline("�Cu�ntas secciones desea tomar?: "))))
  {
    while(is.null(newIni) | is.null(newFin) |
          if(!is.null(newIni) & !is.null(newFin))
          {
            as.Date(newIni, format = "%d/%m/%Y") < iniDates[1] |
              as.Date(newFin, format = "%d/%m/%Y") > finDates[1] |
              as.Date(newIni, format = "%d/%m/%Y") > as.Date(newFin, format = "%d/%m/%Y")
          } else FALSE)
    {
      newIni <- readline(paste("Fecha inicio, secci�n", i, "(DD/MM/AAAA): "))
      newFin <- readline(paste("Fecha final,  secci�n", i, "(DD/MM/AAAA): "))
    }

    iniDates <- c(as.character(iniDates), as.character(as.Date(newIni, format = "%d/%m/%Y")))
    finDates <- c(as.character(finDates), as.character(as.Date(newFin, format = "%d/%m/%Y")))
    newIni <- newFin <- NULL
  }

  return(list(iniDates = iniDates[-1], finDates = finDates[-1]))
}

# Obteneer IGS (�ndice Gonadosom�tico)
.getIndex_IGS <- function(data, sp, ...){

  igsIndex <- data.frame(time = data$date,
                         igs = with(data, wgonad/wevisc*100),
                         stringsAsFactors = FALSE)

  return(igsIndex)
}

# Obtener FC (Factor de condici�n)
.getIndex_FC <- function(data, sp, ...){

  fcIndex <- data.frame(time = data$date,
                        igs = with(data, (wtotal - wgonad)/length^3*100),
                        stringsAsFactors = FALSE)

  return(fcIndex)
}

# Obtener AR (Actividad Reproductiva)
.getIndex_AR <- function(data, sp, ...){

  data <- data[complete.cases(data$mad),]

  nActivos <- gsub(x = unlist(strsplit(species$activos, split = ",")),
                   pattern = " ", replacement = "")
  nActivos <- sum(is.element(data$mad, nActivos))

  arIndex <- nActivos/length(data$mad)*100

  return(arIndex)
}
