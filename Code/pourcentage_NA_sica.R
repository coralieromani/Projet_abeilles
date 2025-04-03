library(readxl)
library(dplyr)

# Charger les données
sica <- read_excel("BD/Activity_Sica_2023.xlsx")

# Exclure la première colonne
sica_sans_premiere_colonne <- sica[, -1]

# Nombre total de valeurs dans le tableau (sans la première colonne)
total_valeurs <- prod(dim(sica_sans_premiere_colonne))

# Nombre de valeurs manquantes
valeurs_manquantes <- sum(is.na(sica_sans_premiere_colonne))

# Pourcentage de valeurs manquantes
pourcentage_manquant <- (valeurs_manquantes / total_valeurs) * 100

# Affichage des résultats
cat("Nombre total de valeurs manquantes :", valeurs_manquantes, "\n")
cat("Pourcentage de valeurs manquantes :", round(pourcentage_manquant, 2), "%\n")
