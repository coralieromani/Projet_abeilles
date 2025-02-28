library(readxl)
library(ggplot2)

poids <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")

#poids de la rûche 412 sur la journée du 04 avril 2023 
poids_restreint <- poids[as.Date(poids$date) == as.Date("2023-04-04"), 
                         c("date", 'Domaine de Confoux (BeeRisk) - Poids de la ruche « Ruche 412 »')]

#Renommer le nom de la colonne car elle contient des caractères spéciaux qui complique la suite
colnames(poids_restreint)[2] <- "poids_ruche412"

#Tracé du graphe 
#(geom_point trace les points, geom_line relie les points, geom_smooth trace la droite de régression linéaire)
ggplot(poids_restreint, aes(x = date, y = poids_ruche412)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(title = "Évolution du poids de la rûche 412 le 04 avril 2023", x = "Heure", y = "Poids de la ruche")

