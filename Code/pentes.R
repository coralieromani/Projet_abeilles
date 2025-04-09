library(readxl)
library(segmented)
library(dplyr)
library(ggplot2)

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

