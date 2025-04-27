library(readxl)
library(segmented)
library(dplyr)
library(ggplot2)
library(purrr)

# Charger les données
poids_ruches <- read_excel("BD/Weight_2023_Sica_Confoux.xlsx")
colnames(poids_ruches) <- c("date", "Confoux - 412", "Confoux - 409", "Confoux - 435",
                            "Confoux - 430", "Confoux - 404", "Confoux - 405",
                            "Sica - 415", "Sica - 428", "Sica - 444",
                            "Sica - 412", "Sica - Y", "Sica - 89")

poids_sica <- poids_ruches [,c(1,8:13)]

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

  modele_seg <- try(segmented(modele, seg.Z = ~heure, npsi = 4), silent = TRUE)

  if (inherits(modele_seg, "try-error")) {
    message(paste("Erreur de segmentation pour le jour:", i))
    jours_erreurs <<- append(jours_erreurs, as.character(as.Date(i)))
    next
  }
  
  breakpoints <- modele_seg$psi[, "Est."]
  breakpoints <- sort(breakpoints)
  
  # Si on n'a pas obtenu 4 breakpoints, on passe ce jour
  if (length(breakpoints) < 4) {
    message(paste("Moins de 4 breakpoints estimés pour le jour:", i))
    jours_erreurs <<- append(jours_erreurs, as.character(as.Date(i)))
    next
  }
  
  # Prédictions aux breakpoints
  poids_predits <- sapply(breakpoints, function(bp) {
    predict(modele_seg, newdata = data.frame(heure = bp))
  })
  
  # Trouver l'indice du poids minimal
  indice_BP2 <- which.min(poids_predits)
  BP2_heure <- breakpoints[indice_BP2]
  
  # Parmi les breakpoints avant BP2, trouver le plus proche
  breakpoints_avant <- breakpoints[breakpoints < BP2_heure]
  
  if (length(breakpoints_avant) == 0) {
    # Cas rare : s'il n'y a pas de breakpoint avant, on saute le jour
    message(paste("Pas de breakpoint avant pour le jour:", i))
    jours_erreurs <<- append(jours_erreurs, as.character(as.Date(i)))
    next
  }
  
  BP1_heure <- breakpoints_avant[which.max(breakpoints_avant)]
  
  # ordonnées aux heures BP1 et BP2
  ordonnee_1 <- predict(modele_seg, newdata = data.frame(heure = BP1_heure))
  ordonnee_2 <- predict(modele_seg, newdata = data.frame(heure = BP2_heure))
  
  format_time <- function(minutes) {
    hours <- floor(as.integer(minutes) / 60)  
    mins <- as.integer(minutes) %% 60     
    return(sprintf("%02d:%02d", hours, mins))
  }

  BP1_time <- format_time(BP1_heure)
  BP2_time <- format_time(BP2_heure)
  
  
  breakpoints_table <- rbind(
    breakpoints_table,
    data.frame(
      date = as.Date(i),
      BP_1 = BP1_time,
      poids_BP_1 = ordonnee_1,
      BP_2 = BP2_time,
      poids_BP_2 = ordonnee_2
    )
  )
}

breakpoints_table <- breakpoints_table %>%
  slice(-1)

# Conversion des heures:minutes en heures
heure_BP_1 <- as.numeric(substr(breakpoints_table$BP_1, 1, 2)) +
  as.numeric(substr(breakpoints_table$BP_1, 4, 5)) / 60
heure_BP_2 <- as.numeric(substr(breakpoints_table$BP_2, 1, 2)) +
  as.numeric(substr(breakpoints_table$BP_2, 4, 5)) / 60

breakpoints_table <- breakpoints_table %>%
  mutate(variation_poids = (poids_BP_2 - poids_BP_1) / (heure_BP_2 - heure_BP_1))

# Conversion des heures en minutes pour recherche
breakpoints_table <- breakpoints_table %>%
  mutate(
    heure_BP_1 = as.numeric(substr(BP_1, 1, 2)) * 60 + as.numeric(substr(BP_1, 4, 5)),
    heure_BP_2 = as.numeric(substr(BP_2, 1, 2)) * 60 + as.numeric(substr(BP_2, 4, 5))
  )

# Sauvegarde
saveRDS(breakpoints_table, "breakpoints_table.rds")

