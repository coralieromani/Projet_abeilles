library(ggplot2)
library(tidyr)
library(dplyr)

# Charger les données
poids_ruches <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")

# Exclure la colonne de date et ne garder que les colonnes des poids des ruches
poids_ruches_sans_date <- poids_ruches %>%
  select(-date)  # Exclure la colonne 'date'

# Transformer les données en format long
poids_long <- poids_ruches_sans_date %>%
  pivot_longer(cols = everything(), names_to = "ruche", values_to = "poids") %>%
  filter(!is.na(poids))  # Filtrer les valeurs manquantes

# Créer le boxplot pour chaque ruche
ggplot(poids_long, aes(x = ruche, y = poids)) +
  geom_boxplot(fill = "pink", color = "purple") +
  labs(title = "Boxplot des poids des ruches", y = "Poids")

# mauve = médiane
# moustache = étendue des données en dehors des quantiles

