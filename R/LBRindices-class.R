plot.LBRindices <- function(data, what = "all", type = "l", ...){

  what <- if(what == "all") c("igs", "fc")

  indexNames <- c("igs", "fc")
  for(i in seq_along(indexNames)){
    with(data$indices[[i]],
         plot(time, igs, type = type, xlab = "Time", ylab = toupper(indexNames[i]),
              main = indexNames[i], ...))
  }

  return(invisible())
}

print.LBRindices <- function(data, what = "all", ...){

  what <- if(what == "all") c("igs", "fc")



  return(invisible())
}
