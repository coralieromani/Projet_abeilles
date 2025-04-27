# Charger les bibliothèques
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)

confoux <- read_excel("BD/Activity_Confoux_2023.xlsx")

# Supprimer les minutes et secondes en arrondissant à l'heure
confoux$date <- parse_date_time(confoux$date, orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM"))
confoux <- confoux %>%
  mutate(across(where(is.numeric), as.character))  # Convertit les nombres en texte

confoux_long <- confoux %>%
  pivot_longer(cols = -date, names_to = "Compteur", values_to = "Valeur")

confoux_missing <- confoux_long %>%
  mutate(Missing = ifelse(is.na(Valeur), "X", "")) 

confoux_missing_periods <- confoux_missing %>%
  group_by(Compteur) %>%
  arrange(date) %>%
  mutate(
    IsMissing = is.na(Valeur),  # Vérifie les NA
    Gap = cumsum(!IsMissing)  # Crée un identifiant de groupe pour chaque période de NA
  ) %>%
  filter(IsMissing) %>%  # Ne garder que les lignes où il y a un NA
  group_by(Compteur, Gap) %>%
  summarise(
    Debut = min(date),
    Fin = max(date),
    .groups = "drop"
  ) %>%
  mutate(Plage = paste0("du ", format(Debut, "%d/%m - %H:%M"), " au ", format(Fin, "%d/%m - %H:%M"))) %>%
  select(Compteur, Plage)

# Supprimer les lignes où le nom du compteur contient le mot "sorties"
confoux_missing_periods_filtered <- confoux_missing_periods %>%
  filter(!grepl("sorties", Compteur, ignore.case = TRUE))  # Exclure les compteurs avec "sorties"


unique_plages <- unique(confoux_missing_periods_filtered$Plage)
unique_compteurs <- unique(confoux_missing_periods_filtered$Compteur)

# Créer un confouxframe vide avec les bonnes dimensions
final_table <- data.frame(Plage = unique_plages)

final_table <- final_table %>%
  mutate(Debut = as.POSIXct(strptime(sub("du (.+) au .*", "\\1", Plage), "%d/%m - %H:%M"))) %>%
  arrange(Debut) %>%
  select(-Debut)  # Supprimer la colonne temporaire utilisée pour le tri

# Vérifier la table triée
print(final_table)

# Ajouter les colonnes des compteurs avec des valeurs vides
for (compteur in unique_compteurs) {
  final_table[[compteur]] <- ""
}

# Remplir le tableau avec des "X"
for (i in 1:nrow(confoux_missing_periods_filtered)) {
  plage <- confoux_missing_periods_filtered$Plage[i]
  compteur <- confoux_missing_periods_filtered$Compteur[i]
  
  final_table[final_table$Plage == plage, compteur] <- "X"
}

saveRDS(final_table, "confoux_donnees_manquantes.rds")

