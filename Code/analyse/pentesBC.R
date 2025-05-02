library(readxl)
library(segmented)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(lubridate)
library(fuzzyjoin)

breakpoints_table <- readRDS("tables/breakpoints_table.rds")
sica_diff <- readRDS("tables/sica_diff.rds")

# Représentation de toutes les pentes des BC

# Création des segments et des points pour le graphique
segments_plot <- data.frame()
points_plot <- data.frame()

for (i in 1:nrow(breakpoints_table)) {
  row <- breakpoints_table[i, ]

  x_start <- row$BP_1
  x_end   <- row$BP_2
  y_start <- 0
  y_end   <- row$poids_BP_2 - row$poids_BP_1  # variation centrée à 0

  segments_plot <- rbind(segments_plot, data.frame(
    x = c(x_start, x_end),
    y = c(y_start, y_end),
    segment_id = i,
    heure_debut = x_start
  ))

  points_plot <- rbind(points_plot, data.frame(
    x = c(x_start, x_end),
    y = c(y_start, y_end),
    point_type = c("BP1", "BP2"),
    segment_id = i
  ))
}

# Graphique avec lignes + points
ggplot() +
  geom_line(data = segments_plot, aes(x = x, y = y, group = segment_id, color = heure_debut), alpha = 0.8, linewidth = 0.8) +
  scale_color_viridis_d() +  # garde les jolies couleurs
  guides(color = "none") +   # masque la légende
  scale_x_discrete(
    name = "Heure de la journée",
    breaks = c("04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00")
  ) +
  labs(
    title = "Pentes entre BP1 et BP2 (Breakfast Canyon)",
    y = "Variation de poids (g, centrée à 0)"
  ) +
  theme_minimal()


# Filtrer les lignes où la différence de poids est supérieure à 1 kg (1000g)
breakpoints_table_filtered <- breakpoints_table %>%
  filter(abs(poids_BP_2 - poids_BP_1) <= 1)

# Création des segments et des points pour le graphique, sur les données filtrées
segments_plot <- data.frame()
points_plot <- data.frame()

for (i in 1:nrow(breakpoints_table_filtered)) {
  row <- breakpoints_table_filtered[i, ]

  x_start <- row$BP_1
  x_end   <- row$BP_2
  y_start <- 0
  y_end   <- row$poids_BP_2 - row$poids_BP_1  # variation centrée à 0

  segments_plot <- rbind(segments_plot, data.frame(
    x = c(x_start, x_end),
    y = c(y_start, y_end),
    segment_id = i,
    heure_debut = x_start
  ))

  points_plot <- rbind(points_plot, data.frame(
    x = c(x_start, x_end),
    y = c(y_start, y_end),
    point_type = c("BP1", "BP2"),
    segment_id = i
  ))
}

# Graphique avec les lignes et les points filtrés
ggplot() +
  geom_line(data = segments_plot, aes(x = x, y = y, group = segment_id, color = heure_debut), alpha = 0.8, linewidth = 0.8) +
  scale_color_viridis_d() +
  guides(color = "none") +
  scale_x_discrete(
    name = "Heure",
    breaks = c("04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00")
  ) +
  labs(
    #title = "Pentes entre BP1 et BP2 (Breakfast Canyon)",
    y = "Variation de poids (en g, centrée à 0)"
  ) +
  theme_minimal()

breakpoints_table_ext <- breakpoints_table %>%
  filter(
    between(pente_ES, -3, 5),
    between(variation_poids, -2.5, 1)
  )

# Filtrer pour la date du 10 avril 2023
sica_10avril <- sica_diff %>% filter(as.Date(date) %in% as.Date(c("2023-04-10")))

bp_10avril <- breakpoints_table_ext %>% filter(as.Date(date) %in% as.Date(c("2023-04-10")))

bp2_10avril = tibble(x=c(bp_10avril$heure_BP_1,bp_10avril$heure_BP_2),y=c(bp_10avril$diff_ES_BP_1,bp_10avril$diff_ES_BP_2))

diff_modele <- lm(diff_ES ~ heure, data = sica_10avril)

ggplot(sica_10avril, aes(x = heure)) +
  geom_ribbon(data = sica_13oct %>% filter(heure >= bp_10avril$heure_BP_1 & heure <= bp_10avril$heure_BP_2),
              aes(ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.1) +
  geom_line(aes(y = diff_ES), color = "skyblue") +
  geom_point(data=bp2_10avril, aes(x=x,y=0), color='red', size=1.9) +
  geom_line(data=bp2_10avril, aes(x=x,y=0), color='red', size=1) +
  labs(
    x = "Heure",
    y = "Différence des Entrées/Sorties"
  ) +
  scale_x_continuous(
    limits = c(0, 1440),  # Plage de 0 à 1440 minutes
    breaks = seq(0, 1440, by = 360), 
    labels = function(x) format(as.POSIXct(x * 60, origin = "1970-01-01", tz = "UTC"), "%H:%M")  # Format HH:MM
  ) +theme_minimal()


# Filtrer pour la date du 13 octobre 2023
sica_13oct <- sica_diff %>% filter(as.Date(date) %in% as.Date(c("2023-10-13")))

bp_13oct <- breakpoints_table_ext %>% filter(as.Date(date) %in% as.Date(c("2023-10-13")))

bp2_13oct = tibble(x=c(bp_13oct$heure_BP_1,bp_13oct$heure_BP_2),y=c(bp_13oct$diff_ES_BP_1,bp_13oct$diff_ES_BP_2))

diff_modele <- lm(diff_ES ~ heure, data = sica_13oct)

ggplot(sica_13oct, aes(x = heure)) +
  # Transparence
  geom_ribbon(data = sica_13oct %>% filter(heure >= bp_13oct$heure_BP_1 & heure <= bp_13oct$heure_BP_2),
              aes(ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.1) +
  geom_line(aes(y = diff_ES), color = "skyblue") +
  geom_point(data = bp2_13oct, aes(x = x, y = 0), color = 'red', size = 1.9) +
  geom_line(data = bp2_13oct, aes(x = x, y = 0), color = 'red', size = 1) +
  labs(x = "Heure", y = "Différence des Entrées/Sorties") +
  scale_x_continuous(
    limits = c(0, 1440),  # Plage de 0 à 1440 minutes
    breaks = seq(0, 1440, by = 360), 
    labels = function(x) format(as.POSIXct(x * 60, origin = "1970-01-01", tz = "UTC"), "%H:%M")
  ) +
  theme_minimal()
