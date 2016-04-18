# Nombres de variables
.varsNames <- read.csv(file = "auxiliar/var_names.csv", stringsAsFactors = FALSE)
save(.varsNames, file = "data/varsnames.RData")

# Variables necesarias para cada índice
.indexDepends <- read.csv(file = "auxiliar/index_depends.csv", stringsAsFactors = FALSE)
save(.indexDepends, file = "data/inde_depends.RData")
