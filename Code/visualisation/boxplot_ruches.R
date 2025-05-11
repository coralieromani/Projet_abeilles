library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)

# Charger les données
poids_ruches <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")

colnames(poids_ruches) <- c("date", "412 ", "409", "435",
                            "430", "404", "405",
                            "415", "428", "444",
                            "412", "Y", "89")

poids_sansdate <- poids_ruches[2:13]

#Suppression de la colonne Date et des rûches de Confoux
poids_sica_sansdate <- poids_sansdate[7:12]

#Suppression de la colonne Date et des rûches de Sica
poids_confoux_sansdate <- poids_sansdate[1:6]

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
  labs(y = "Poids (en g)", x ="Ruches")

ggplot(poids_long_sica, aes(x = ruche, y = poids)) +
  geom_boxplot(fill = "pink", color = "purple") +
  labs(y = "Poids (en g)", x = "Ruches")

# mauve = médiane
# moustache = étendue des données en dehors des quantiles

