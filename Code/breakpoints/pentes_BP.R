library(readxl)
library(segmented)
library(dplyr)
library(ggplot2)

breakpoints_table <- readRDS("tables/breakpoints_table.rds")
poids_moyen <- readRDS("tables/poids_moyen.rds")

test <- subset(poids_moyen, date == "2023-04-22")
modele <- lm(poids ~ heure, data = test)
modele_seg <- segmented(modele, seg.Z = ~heure, npsi = 4)

test$heure_POSIX <- as.POSIXct(sprintf("%02d:%02d", test$heure %/% 60, test$heure %% 60), format = "%H:%M")
plot(test$heure_POSIX, test$poids, type = "b",
     xlab = "Heure", ylab = "Poids", 
     xaxt = "n")  # Supprimer l'axe X par défaut
axis.POSIXct(1, at = pretty(test$heure_POSIX), format = "%H:%M")
lines(test$heure_POSIX, predict(modele_seg), col = "red", lwd = 2)

for (i in jours_erreurs) {
  erreur <- subset(poids_moyen, date == as.Date(i))  
  modele <- lm(poids ~ heure, data = erreur)
  modele_seg <- segmented(modele, seg.Z = ~heure, npsi = 1)
  erreur$heure_POSIX <- as.POSIXct(sprintf("%02d:%02d", erreur$heure %/% 60, erreur$heure %% 60), format = "%H:%M", tz = "UTC")
  plot(erreur$heure_POSIX, erreur$poids, type = "b", 
       #main = paste("Régression segmentée le", i),
       xlab = "Heure", ylab = "Poids", xaxt = "n")
  
  axis.POSIXct(1, at = pretty(erreur$heure_POSIX), format = "%H:%M", tz = "UTC")
  lines(erreur$heure_POSIX, predict(modele_seg), col = "red", lwd = 2)
}

ggplot(breakpoints_table) +
  geom_point(aes(x = date, y = poids_BP_1, color = "BP_1"), alpha = 1) +
  geom_point(aes(x = date, y = poids_BP_2, color = "BP_2"), alpha = 1) +
  labs(title = "Évolution des breakpoints", x = "Temps", y = "Poids") +
  scale_y_continuous(limits = c(35, 55)) +
  scale_color_manual(values = c("BP_1" = "blue", "BP_2" = "orange"), name = "Breakpoints") +
  theme_minimal()

breakpoints_table$BP_1_numeric <- as.numeric(substr(breakpoints_table$BP_1, 1, 2)) +
  as.numeric(substr(breakpoints_table$BP_1, 4, 5)) / 60
breakpoints_table$BP_2_numeric <- as.numeric(substr(breakpoints_table$BP_2, 1, 2)) +
  as.numeric(substr(breakpoints_table$BP_2, 4, 5)) / 60

# Régressions
lm_BP1 <- lm(BP_1_numeric ~ as.numeric(date), data = breakpoints_table)
lm_BP2 <- lm(BP_2_numeric ~ as.numeric(date), data = breakpoints_table)

# R²
R2_BP1 <- round(summary(lm_BP1)$r.squared, 3)
R2_BP2 <- round(summary(lm_BP2)$r.squared, 3)

# Droites de régression
breakpoints_table$BP_1_trend <- predict(lm_BP1, newdata = breakpoints_table)
breakpoints_table$BP_2_trend <- predict(lm_BP2, newdata = breakpoints_table)

# Position pour les annotations
max_date <- max(breakpoints_table$date)
min_date <- min(breakpoints_table$date)
annot_x <- min_date + (max_date - min_date) * 0.7

# Graphique
ggplot(breakpoints_table, aes(x = date)) +
  geom_point(aes(y = BP_1_numeric, color = "Breakpoint B")) +
  geom_point(aes(y = BP_2_numeric, color = "Breakpoint C")) +
  geom_line(aes(y = BP_1_trend), color = "darkgreen", size = 1) +
  geom_line(aes(y = BP_2_trend), color = "#D5006D", size = 1) +
  labs(x = "Jour", y = "Heure") +
  scale_y_continuous(breaks = seq(0, 24, by = 1), limits = c(0, 24)) +
  scale_color_manual(values = c("Breakpoint B" = "green", "Breakpoint C" = "pink"), name = "Breakpoints") +
  theme_minimal()

ggplot(breakpoints_table, aes(x = date, y = variation_poids)) +
  geom_point(color = "blue") +
  labs(x = "Jour", y = "Pente en gramme par heure") +
  scale_y_continuous(breaks = seq(min(breakpoints_table$variation_poids),
                                  max(breakpoints_table$variation_poids),
                                  by = 1)) +
  theme_minimal()

breakpoints_table_filtre <- breakpoints_table %>%
  filter(!date %in% as.Date(c("2023-06-05","2023-04-28","2023-09-18")))

modele_pente <- lm(variation_poids ~ as.numeric(date), data = breakpoints_table_filtre)

# Résumé du modèle pour extraire le R²
R2_pente = summary(modele_pente)$r.squared

ggplot(breakpoints_table_filtre, aes(x = date, y = variation_poids)) +
  geom_point(color = "blue", size = 1) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(x = "Jour", y = "Pente en gramme par heure") +
  scale_y_continuous(limits = c(-0.7, 0),
                     breaks = seq(-0.7, 0, by = 0.1)
  ) +
  theme_minimal()


