# Nombres de variables
.varsNames <- read.csv(file = "auxiliar/var_names.csv", stringsAsFactors = FALSE)
save(.varsNames, file = "data/varsnames.RData")

# Variables necesarias para cada índice
.indexDepends <- read.csv(file = "auxiliar/index_depends.csv", stringsAsFactors = FALSE)
save(.indexDepends, file = "data/index_depends.RData")

# Parámetros por especie
species <- read.csv(file = "auxiliar/species.csv", stringsAsFactors = FALSE)
save(species, file = "data/species.RData")
