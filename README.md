# 🐝 Projet Abeilles : Analyse des variations journalières de poids des ruches d’abeilles domestiques

Ce projet a pour objectif principal d’étudier le phénomène du **Breakfast Canyon**, une phase caractéristique de perte de poids des ruches. À travers cette analyse, nous cherchons à mieux comprendre le comportement des butineuses, en particulier en liant l’activité des entrées et sorties des abeilles avec les variations de poids mesurées au cours de l’année 2023.

## Objectifs 

- Étudier les variation de poids des rûches durant le **Breakfast Canyon**.
- Identifier et caractériser le **Breakfast Canyon** à partir de sa pente et des breakpoints qui la délimitent. 
- Évaluer l’impact de cette phase sur l'activité des butineuses.
- Explorer les variations saisonnières (par mois) de cette activité.

## Méthodologie

L’ensemble de notre étude repose sur l’application de *régressions linéaires segmentées*.
En particulier, nous l'avons appliqué aux données de poids des ruches pour l'extraction des breakpoints du **Breakfast Canyon**, mais également à d'autres variables pour approfondire notre étude.

## Résultats

Les résultats sont visualisés sous forme de graphiques, sous forme de moyenne et de somme, mettant en relation :
- Les pentes de perte de poids du **Breakfast Canyon**
- Les entrées/sorties
- Les différences d’entrées/sorties


## Outils utilisés

- Langage : **R** pour l’analyse statistique et la visualisation
- Bibliothèques : **ggplot2**, **segmented**, **dplyr**, **lubridate**
- Éditeurs : **RStudio**, **GitHub**

## Références

Des résumés d’articles scientifiques liés à la thématique sont disponibles dans le dossier `Articles/`.
Les bases de données utilisées sont dans le dossier `BD\`.

## Auteurs

Projet réalisé par [Ton Nom ou Équipe], dans le cadre de notre Master 1 Statistiques et Sciences des Données.

---

