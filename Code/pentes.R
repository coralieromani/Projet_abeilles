library(readxl)
library(segmented)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(lubridate)
library(fuzzyjoin)


# Pentes breakpoints

poids_ruches <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")
colnames(poids_ruches) <- c("date", "Confoux - 412", "Confoux - 409", "Confoux - 435",
                            "Confoux - 430", "Confoux - 404", "Confoux - 405",
                            "Sica - 415", "Sica - 428", "Sica - 444",
                            "Sica - 412", "Sica - Y", "Sica - 89")

poids_sica <- poids_ruches[,1-8:13]
poids_sica$date <- as.POSIXct(poids_sica$date,format = "%Y-%m-%d %H:%M:%S")
poids_sica$heure <- as.numeric(format(poids_sica$date, "%H")) * 60 + as.numeric(format(poids_sica$date, "%M"))
poids_sica$date <- as.Date(poids_sica$date)
poids_sica <- poids_sica %>%
  filter(date >="2023-04-04" & date <= "2023-11-23")

poids_moyen <- data.frame(date = poids_sica$date, heure = poids_sica$heure, poids = rowMeans(poids_sica[, -c(1, ncol(poids_sica))], na.rm = TRUE))
#

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

breakpoints_table <- breakpoints_table %>%
  slice(-1)

#Conversion des heures:minutes en heures (ex: 07:30 devient 7,5)
BP_1_numeric <- as.numeric(substr(breakpoints_table$BP_1, 1, 2)) +
  as.numeric(substr(breakpoints_table$BP_1, 4, 5)) / 60
BP_2_numeric <- as.numeric(substr(breakpoints_table$BP_2, 1, 2)) +
  as.numeric(substr(breakpoints_table$BP_2, 4, 5)) / 60

breakpoints_table <- breakpoints_table %>%
  mutate(pente_bp = (poids_BP_2 - poids_BP_1) / (BP_2_numeric - BP_1_numeric))


# Pentes des entrées - sorties
sica <- read_excel("BD/Activity_Sica_2023.xlsx")

sica_long <- sica %>%
  pivot_longer(cols = -date, names_to = "type", values_to = "valeur") %>%
  mutate(
    valeur = as.numeric(valeur),
    ruche = sub("CPT_|-entrées|-sorties", "", type),
    Mouvement = ifelse(grepl("entrées", type), "Entrées", "Sorties")
  ) %>%
  filter(!is.na(valeur)) %>%  # Écarte les valeurs manquantes
  group_by(date, Mouvement) %>%
  summarise(moyenne = mean(valeur, na.rm = TRUE), .groups = "drop")

sica_diff <- sica_long %>%
  pivot_wider(names_from = Mouvement, values_from = moyenne) %>%
  mutate(diff_ES = Entrées - Sorties)

sica_diff <- sica_diff %>%
  mutate(
    heure = as.numeric(format(date, "%H")) * 60 + as.numeric(format(date, "%M"))
  ) %>% summarise(date=as.Date(date),heure,diff_ES)


# Fonction modifiée : ne retourne jamais NA, prend la valeur la plus proche
get_diff_ES_at_time <- function(current_date, target_heure) {
  sica_day <- sica_diff %>% filter(date == current_date)
  if (nrow(sica_day) == 0) return(NA)
  closest_row <- sica_day[which.min(abs(sica_day$heure - target_heure)), ]
  return(closest_row$diff_ES)
}

# Conversion des heures en minutes pour recherche
breakpoints_table <- breakpoints_table %>%
  mutate(
    heure_BP_1 = as.numeric(substr(BP_1, 1, 2)) * 60 + as.numeric(substr(BP_1, 4, 5)),
    heure_BP_2 = as.numeric(substr(BP_2, 1, 2)) * 60 + as.numeric(substr(BP_2, 4, 5))
  )

# Ajouter les valeurs diff_ES correspondantes
breakpoints_table <- breakpoints_table %>%
  mutate(
    diff_ES_BP_1 = mapply(get_diff_ES_at_time, date, heure_BP_1),
    diff_ES_BP_2 = mapply(get_diff_ES_at_time, date, heure_BP_2)
  )

breakpoints_table <- breakpoints_table %>%
  mutate(pente_ES = (diff_ES_BP_2 - diff_ES_BP_1) / (heure_BP_2 - heure_BP_1))
         
ggplot(breakpoints_table, aes(x = pente_ES, y = pente_bp)) +
  geom_point() +
  theme_minimal()

breakpoints_table_ext <- breakpoints_table %>%
  filter(
    between(pente_ES, -3, 5),
    between(pente_bp, -2.5, 1)
  )

ggplot(breakpoints_table_ext, aes(x = pente_ES, y = pente_bp)) +
  geom_point() +
  theme_minimal()


       
# Filtrer pour la date du 10 avril 2023
sica_10avril <- sica_diff %>% filter(as.Date(date) %in% as.Date(c("2023-04-10")))

bp_10avril <- breakpoints_table_ext %>% filter(as.Date(date) %in% as.Date(c("2023-04-10")))

bp2_10avril = tibble(x=c(bp_10avril$heure_BP_1,bp_10avril$heure_BP_2),y=c(bp_10avril$diff_ES_BP_1,bp_10avril$diff_ES_BP_2))

diff_modele <- lm(diff_ES ~ heure, data = sica_10avril)

ggplot(sica_10avril, aes(x = heure)) +
  geom_line(aes(y = diff_ES), color = "skyblue") +
  geom_point(data=bp2_10avril, aes(x=x,y=y), color='red', size=1.9) +
  geom_line(data=bp2_10avril, aes(x=x,y=y), color='red', size=1) +
  labs(
    title = "Régression segmentée : ENTRÉES - SORTIES le 10 avril 2023",
    x = "Heure",
    y = "Différence"
  ) +theme_minimal()


# Filtrer pour la date du 13 octobre 2023
sica_13oct <- sica_diff %>% filter(as.Date(date) %in% as.Date(c("2023-10-13")))

bp_13oct <- breakpoints_table_ext %>% filter(as.Date(date) %in% as.Date(c("2023-10-13")))

bp2_13oct = tibble(x=c(bp_13oct$heure_BP_1,bp_13oct$heure_BP_2),y=c(bp_13oct$diff_ES_BP_1,bp_13oct$diff_ES_BP_2))

diff_modele <- lm(diff_ES ~ heure, data = sica_13oct)

ggplot(sica_13oct, aes(x = heure)) +
  geom_line(aes(y = diff_ES), color = "skyblue") +
  geom_point(data=bp2_13oct, aes(x=x,y=y), color='red', size=1.9) +
  geom_line(data=bp2_13oct, aes(x=x,y=y), color='red', size=1) +
  labs(
    title = "Régression segmentée : ENTRÉES - SORTIES le 13 octobre 2023",
    x = "Heure",
    y = "Différence"
  ) + theme_minimal()
