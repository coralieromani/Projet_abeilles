library(dplyr)
library(readxl)

# Lecture du fichier
poids <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")

# Renommer les colonnes
colnames(poids) <- c("date", "Confoux - 412", "Confoux - 409", "Confoux - 435",
                     "Confoux - 430", "Confoux - 404", "Confoux - 405",
                     "Sica - 415", "Sica - 428", "Sica - 444",
                     "Sica - 412", "Sica - Y", "Sica - 89")

# Séparer les données en deux sous-ensembles
confoux_cols <- poids %>% select(starts_with("Confoux"))
sica_cols    <- poids %>% select(starts_with("Sica"))

# Fonction pour compter les NA et le pourcentage
calcul_NA <- function(df_part) {
  total_valeurs <- prod(dim(df_part))
  valeurs_manquantes <- sum(is.na(df_part))
  pourcentage_manquant <- (valeurs_manquantes / total_valeurs) * 100
  return(list(nb_na = valeurs_manquantes, pct_na = round(pourcentage_manquant, 2)))
}

# Calcul pour chaque groupe
confoux_na <- calcul_NA(confoux_cols)
sica_na    <- calcul_NA(sica_cols)

# Affichage
cat("Confoux :\n")
cat(" - Nombre de valeurs manquantes :", confoux_na$nb_na, "\n")
cat(" - Pourcentage :", confoux_na$pct_na, "%\n\n")

cat("Sica :\n")
cat(" - Nombre de valeurs manquantes :", sica_na$nb_na, "\n")
cat(" - Pourcentage :", sica_na$pct_na, "%\n")
