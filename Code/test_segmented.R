library(readxl)
library(ggplot2)
library(segmented)

# Charger les données
poids <- read_excel("Weight_2023_Sica_Confoux.xlsx")

# Poids de la ruche 412 pour le 04 avril 2023
poids_restreint <- poids[as.Date(poids$date) == as.Date("2023-06-04"), 
                         c("date", 'Domaine de Confoux (BeeRisk) - Poids de la ruche « Ruche 412 »')]

# Renommer la colonne
colnames(poids_restreint)[2] <- "poids_ruche412"

# Convertir la date en format numérique pour l'utiliser dans la régression
poids_restreint$date_num <- as.numeric(poids_restreint$date)

# Ajuster un modèle linéaire simple
modele <- lm(poids_ruche412 ~ date_num, data = poids_restreint)

# Ajuster le modèle linéaire segmenté (ajuster la rupture)
modele_segmenté <- segmented(modele, seg.Z = ~date_num, npsi = 3)


# Tracer le graphique avec la régression linéaire segmentée
ggplot(poids_restreint, aes(x = date, y = poids_ruche412)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +  # Régression linéaire globale
  geom_line(aes(y = predict(modele_segmenté, newdata = poids_restreint)), color = "blue") + # Régression linéaire segmentée
  labs(title = "Évolution du poids de la ruche 412 le 04 avril 2023", x = "Heure", y = "Poids de la ruche") +
  theme_minimal()

ggplot(poids_restreint, aes(x = date, y = poids_ruche412)) +
  geom_line(aes(y = predict(modele_segmenté, newdata = poids_restreint)), color = "blue")
