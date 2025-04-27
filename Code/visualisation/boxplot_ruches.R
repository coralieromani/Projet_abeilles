library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)

# Charger les données
poids_ruches <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")

colnames(poids_ruches) <- c("date", "Confoux - 412", "Confoux - 409", "Confoux - 435",
                            "Confoux - 430", "Confoux - 404", "Confoux - 405",
                            "Sica - 415", "Sica - 428", "Sica - 444",
                            "Sica - 412", "Sica - Y", "Sica - 89")

poids_sansdate <- poids_ruches %>%
  select(-1)

#Suppression de la colonne Date et des rûches de Confoux
poids_sica_sansdate <- poids_sansdate %>%
  select((ncol(poids_sansdate)-5):ncol(poids_sansdate))

#Suppression de la colonne Date et des rûches de Sica
poids_confoux_sansdate <- poids_sansdate %>%
  select(1:6)

# Transformer les données en format long

poids_long_confoux <- poids_confoux_sansdate%>%
  pivot_longer(cols = everything(), names_to = "ruche", values_to = "poids") %>%
  filter(!is.na(poids))  # Filtrer les valeurs manquantes

poids_long_sica <- poids_sica_sansdate%>%
  pivot_longer(cols = everything(), names_to = "ruche", values_to = "poids") %>%
  filter(!is.na(poids))  # Filtrer les valeurs manquantes

#Tracé des boxplot
ggplot(poids_long_confoux, aes(x = ruche, y = poids)) +
  geom_boxplot(fill = "pink", color = "purple") +
  labs(y = "Poids", x ="Rûches")

ggplot(poids_long_sica, aes(x = ruche, y = poids)) +
  geom_boxplot(fill = "pink", color = "purple") +
  labs(y = "Poids", x = "Rûches")

# mauve = médiane
# moustache = étendue des données en dehors des quantiles

