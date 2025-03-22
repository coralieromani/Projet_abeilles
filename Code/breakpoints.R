library(readxl)
library(segmented)
library(dplyr)
library(ggplot2)

# Charger les données
poids_ruches <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")
poids_ruches$date <- as.POSIXct(poids_ruches$date,format = "%Y-%m-%d %H:%M:%S")
poids_ruches$heure <- as.numeric(format(poids_ruches$date, "%H")) * 60 + as.numeric(format(poids_ruches$date, "%M"))
poids_ruches$date <- as.Date(poids_ruches$date)
poids_ruches <- poids_ruches %>%
  filter(date >="2023-04-04" & date <= "2023-11-23")

poids_moyen <- data.frame(date = poids_ruches$date, heure = poids_ruches$heure, poids = rowMeans(poids_ruches[, -c(1, ncol(poids_ruches))], na.rm = TRUE)) 


# Initialiser une table vide pour stocker les breakpoints
breakpoints_table <- data.frame(
  date = as.Date(NA),
  BP_1 = character(1), 
  poids_BP_1 = numeric(1), 
  BP_2 = character(1), 
  poids_BP_2 = numeric(1), 
  stringsAsFactors = FALSE
)

jours_erreurs <- list()

for (i in unique(poids_moyen$date)) {
  poids_jour <- subset(poids_moyen, date == i)
  
  modele <- lm(poids ~ heure, data = poids_jour)
  
  modele_seg <- try(segmented(modele, seg.Z = ~heure, psi = c(400,800)), silent = TRUE)
  
  breakpoints <- modele_seg$psi[, "Est."]
  
  # Si on n'a pas obtenu 2 breakpoints, on passe ce jour
  if (length(breakpoints) < 2) {
    message(paste("Moins de 2 breakpoints estimés pour le jour:", i))
    jours_erreurs <<- append(jours_erreurs, as.character(as.Date(i)))
    next
  }
  
  format_time <- function(minutes) {
    hours <- floor(as.integer(minutes) / 60)  # Conversion explicite en entier
    mins <- as.integer(minutes) %% 60        # Conversion explicite en entier
    return(sprintf("%02d:%02d", hours, mins))
  }
  
  
  BP_1_time <- format_time(breakpoints[1])
  BP_2_time <- format_time(breakpoints[2])
  
  ordonnee_1 <- predict(modele_seg, newdata = data.frame(heure = breakpoints[1]))
  ordonnee_2 <- predict(modele_seg, newdata = data.frame(heure = breakpoints[2]))
  
  breakpoints_table <- rbind(
    breakpoints_table, 
    data.frame(
      date = as.Date(i),
      BP_1 = BP_1_time, 
      poids_BP_1 = ordonnee_1, 
      BP_2 = BP_2_time, 
      poids_BP_2 = ordonnee_2
    )
  )
}


test <- subset(poids_moyen, date == "2023-05-06")
modele <- lm(poids ~ heure, data = test)
modele_seg <- segmented(modele, seg.Z = ~heure, npsi = 2)
plot(test$heure, test$poids, type = "b", main = paste("Régression segmentée le 2023-04-06"), xlab = "Heure", ylab = "Poids")
lines(test$heure, predict(modele_seg), col = "red", lwd = 2)

for (i in jours_erreurs) {
  erreur <- subset(poids_moyen, date == i)
  modele <- lm(poids ~ heure, data = erreur)
  modele_seg <- segmented(modele, seg.Z = ~heure, npsi = 1)
  plot(erreur$heure, erreur$poids, type = "b", main = paste("Régression segmentée le", i), xlab = "Heure", ylab = "Poids")
  lines(erreur$heure, predict(modele_seg), col = "red", lwd = 2)
}


ggplot(breakpoints_table) +
  geom_point(aes(x = date, y = poids_BP_1, color = "BP_1"), alpha = 1) +
  geom_point(aes(x = date, y = poids_BP_2, color = "BP_2"), alpha = 1) +
  labs(title = "Évolution des breakpoints", x = "Temps", y = "Poids") +
  scale_y_continuous(limits = c(35, 55)) +
  scale_color_manual(values = c("BP_1" = "blue", "BP_2" = "orange"), name = "Breakpoints") +  
  theme_minimal()
