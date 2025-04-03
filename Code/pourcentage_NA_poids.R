library(dplyr)
library(readxl)

poids <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")

colnames(poids) <- c("date", "Confoux - 412", "Confoux - 409", "Confoux - 435",
                            "Confoux - 430", "Confoux - 404", "Confoux - 405",
                            "Sica - 415", "Sica - 428", "Sica - 444",
                            "Sica - 412", "Sica - Y", "Sica - 89")

total_valeurs <- prod(dim(poids))

# Nombre de valeurs manquantes
valeurs_manquantes <- sum(is.na(poids))

# Pourcentage de valeurs manquantes
pourcentage_manquant <- (valeurs_manquantes / total_valeurs) * 100

# Affichage des résultats
cat("Nombre total de valeurs manquantes :", valeurs_manquantes, "\n")
cat("Pourcentage de valeurs manquantes :", round(pourcentage_manquant, 2), "%\n")
