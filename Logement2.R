library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(stringr)
library(fuzzyjoin)
library(forecast)

# Chargement du fichier Excel -----

logement <- read_xlsx("C:/Users/jeremie.dupont/Desktop/Stage/Logement/BDD stage.xlsx") %>%
  fill(`Résidence`, .direction = "down") %>%
  filter(!is.na(`Résidence`)) %>% 
  filter(!(`Résidence` == "Résidence Jean Zay" & `Année de gestion` == 2022)) %>% 
  filter(!(`Résidence` == "Résidence Gaston Adriensence" & `Année de gestion` == 2023)) %>% 
  mutate(
    `Secteur` = case_when(
      `Résidence` %in% c(
        "Résidence Moulins-Parc Centre", "Résidence Arsenal des Postes", "Cité Albert Châtelet",
        "Résidence Courmont", "Résidence Jean Zay", "Résidence Guy de Maupassant",
        "Résidence Georges Lefèvre", "Résidence Fives", "Cité Robespierre", "Cité Bas-Liévin"
      ) ~ "Lille Centre",
      TRUE ~ `Secteur`
    )
  ) %>%
  mutate(
    `Secteur` = case_when(
      `Résidence` ==
        "Cité Jean Mermoz" ~ "Lille Est",
      TRUE ~ `Secteur`
    )
  ) %>%
  mutate(`Secteur` = case_when(
    `Secteur` %in% c("Lille-Villeneuve d'Ascq", "Campus Pont de Bois", "Campus Cité Scientifique") ~ "Lille Est",
    `Secteur` %in% c("Campus Moulins-Ronchin", "Campus Santé") ~ "Lille Centre",
    TRUE ~ `Secteur`
    )
  ) %>% 
  mutate(
    `Sous-phase (Libellé)` = case_when(
    `Sous-phase (Libellé)` == "2024 sous-phase 1" ~ "Tour 1",
    `Sous-phase (Libellé)` == "2024 sous-phase 2" ~ "Tour 2",
    `Sous-phase (Libellé)` == "2024 sous-phase 3" ~ "Tour 3",
    `Sous-phase (Libellé)` == "2024 sous-phase 4" ~ "Tour 4",
    `Sous-phase (Libellé)` == "Sous-Phase 1" ~ "Tour 1",
    `Sous-phase (Libellé)` == "Sous-Phase 2" ~ "Tour 2",
    `Sous-phase (Libellé)` == "Sous-Phase 3" ~ "Tour 3",
    `Sous-phase (Libellé)` == "Sous-Phase 4" ~ "Tour 4",
    `Sous-phase (Libellé)` == "Tour Unique 2021" ~ "Tour 1",
      TRUE ~ `Sous-phase (Libellé)` 
    ) 
  ) %>% 
  mutate(`Places Tour 1` = 0
         ) %>% 
  mutate(`Places Tour 2` = 0
  ) %>% 
  mutate(`Places Tour 3` = 0
  ) %>% 
  mutate(`Places Tour 4` = 0
  ) %>% 
  mutate(`Places Total` = 0
  ) %>% 
  mutate(`Places phase complémentaire` = 0) %>% 
    mutate(
      Latitude = case_when(
        Secteur == "Arras" ~ 50.2910,
        Secteur == "Béthune" ~ 50.5300,
        Secteur == "Boulogne-sur-Mer" ~ 50.7264,
        Secteur == "Calais" ~ 50.9513,
        Secteur == "Cambrai" ~ 50.1760,
        Secteur == "Lens-Liévin" ~ 50.4320,
        Secteur == "Lille Centre" ~ 50.6292,
        Secteur == "Lille Est" ~ 50.6329,
        Secteur == "Longuenesse - Saint Omer" ~ 50.7515,
        Secteur == "Maubeuge" ~ 50.2782,
        Secteur == "Roubaix - Tourcoing" ~ 50.6916,
        Secteur == "Valenciennes" ~ 50.3498,
        TRUE ~ NA_real_
      )) %>%
  mutate(Longitude = case_when(
    Secteur == "Arras" ~ 2.7775,
    Secteur == "Béthune" ~ 2.6320,
    Secteur == "Boulogne-sur-Mer" ~ 1.6133,
    Secteur == "Calais" ~ 1.8587,
    Secteur == "Cambrai" ~ 3.2342,
    Secteur == "Lens-Liévin" ~ 2.8332,
    Secteur == "Lille Centre" ~ 3.0573,
    Secteur == "Lille Est" ~ 3.1168,
    Secteur == "Longuenesse - Saint Omer" ~ 2.2467,
    Secteur == "Maubeuge" ~ 3.9726,
    Secteur == "Roubaix - Tourcoing" ~ 3.1746,
    Secteur == "Valenciennes" ~ 3.3464,
    TRUE ~ NA_real_
  )
  )
  

# Fonction modification----
standardiser_residence <- function(data, colonne_residence = "Résidence") {
  data %>%
    mutate(
      !!colonne_residence := case_when(
        !!sym(colonne_residence) == "Camus" ~ "Albert Camus",
        !!sym(colonne_residence) == "Châtelet" ~ "Albert Châtelet",
        !!sym(colonne_residence) == "Bachelard" ~ "Gaston Bachelard",
        !!sym(colonne_residence) == "Boucher" ~ "Hélène Boucher",
        !!sym(colonne_residence) == "Mermoz" ~ "Jean Mermoz",
        !!sym(colonne_residence) == "Mousseron" ~ "Jules Mousseron",
        !!sym(colonne_residence) == "E. Moreau" ~ "Emilienne Moreau",
        !!sym(colonne_residence) == "G. Eiffel" ~ "Gustave Eiffel",
        !!sym(colonne_residence) == "G.Philipe" ~ "Gérard Philipe",
        !!sym(colonne_residence) == "Artois" ~ "de l Artois",
        !!sym(colonne_residence) == "E. Moreau" ~ "Emilienne Moreau",
        !!sym(colonne_residence) == "Moreau" ~ "Emilienne Moreau",
        !!sym(colonne_residence) == "BARJAVEL" ~ "René Barjavel",
        !!sym(colonne_residence) == "Bernanos" ~ "Georges Bernanos",
        !!sym(colonne_residence) == "Du Vivier" ~ "du Vivier",
        !!sym(colonne_residence) == "Ansart" ~ "Gustave Ansart",
        !!sym(colonne_residence) == "Lefevre" ~ "Georges Lefèvre",
        !!sym(colonne_residence) == "Liberté" ~ "La Liberté",
        !!sym(colonne_residence) == "Marmottan" ~ "Jules Marmottan",
        !!sym(colonne_residence) == "Maupassant" ~ "Guy de Maupassant",
        !!sym(colonne_residence) == "Moulins parc centre" ~ "Moulins-Parc Centre",
        !!sym(colonne_residence) == "Pont de bois" ~ "Pont de Bois",
        !!sym(colonne_residence) == "Saint-Roch" ~ "Saint Roch",
        !!sym(colonne_residence) == "Bas-liévin" ~ "Bas-Liévin",
        TRUE ~ !!sym(colonne_residence)
      )
    ) %>%
    mutate(
      !!colonne_residence := case_when(
        # Ajouter "Cité" pour les résidences spécifiques
        !!sym(colonne_residence) %in% c(
          "Albert Camus", "Albert Châtelet", "Bas-Liévin", "Evariste", 
          "Gaston Bachelard", "Gérard Philipe", "Hélène Boucher", 
          "Jean Mermoz", "Jules Mousseron", "Les Templiers", 
          "Robespierre", "Triolo"
        ) ~ paste("Cité", !!sym(colonne_residence)),
        # Ajouter "Résidence" pour les autres noms
        TRUE ~ paste("Résidence", !!sym(colonne_residence))
      )
    )
}
# Définir une fonction pour traiter les données d'une année spécifique
traiter_logement_par_annee <- function(logement, logement_annee, annee_gestion) {
  logement %>%
    # Filtrer uniquement les données pour l'année spécifiée
    filter(`Année de gestion` == annee_gestion) %>%
    # Faire une jointure avec uniquement les colonnes nécessaires
    left_join(
      logement_annee %>% select(`Résidence`, `PLACES1`, `PLACES2`, `PLACES3`, `PLACES4`, `PhaseC`),
      by = "Résidence"
    ) %>%
    # Mettre à jour les colonnes "Places Tour"
    mutate(`Places Tour 1` = ifelse(`Sous-phase (Libellé)` == "Tour 1" & !is.na(`PLACES1`), PLACES1, `Places Tour 1`)) %>%
    mutate(`Places Tour 2` = ifelse(`Sous-phase (Libellé)` == "Tour 2" & !is.na(`PLACES2`), PLACES2, `Places Tour 2`)) %>%
    mutate(`Places Tour 3` = ifelse(`Sous-phase (Libellé)` == "Tour 3" & !is.na(`PLACES3`), PLACES3, `Places Tour 3`)) %>%
    mutate(`Places Tour 4` = ifelse(`Sous-phase (Libellé)` == "Tour 4" & !is.na(`PLACES4`), PLACES4, `Places Tour 4`)) %>%
    mutate(`Places phase complémentaire` = `PhaseC`) %>% 
    # Supprimer les colonnes temporaires
    select(-`PLACES1`, -`PLACES2`, -`PLACES3`, -`PLACES4`,-`PhaseC`) %>%
    # Calculer la colonne "Places Total"
    mutate(
      `Places Total` = `Places Tour 1`
      )
}

# Réintégrer les données pour toutes les années
traiter_toutes_les_annees <- function(logement, logement_par_annee, annees) {
  # Traiter chaque année et combiner les résultats
  logement_traite <- purrr::map_dfr(annees, function(annee) {
    traiter_logement_par_annee(logement, logement_par_annee[[as.character(annee)]], annee)
  })
  
  # Ajouter les autres années qui ne sont pas dans la liste traitée
  logement_autres_annees <- logement %>%
    filter(!`Année de gestion` %in% annees)
  
  # Combiner le tout et trier par année de gestion
  bind_rows(logement_traite, logement_autres_annees) %>%
    arrange(`Année de gestion`)
}

################################################
########      Modification Fichier      ########
################################################
#Logement Dispo-----
logementdispo <- read_xlsx("C:/Users/jeremie.dupont/Desktop/Stage/Logement/LogementDispo.xlsx")

logementdispo <- logementdispo %>%
  filter(!is.na(`Résidence`), !is.na(`Nombre logement`))

logementdispo <- logementdispo %>%
  filter(!is.na(`Résidence`)) %>% 
  filter(`Résidence` != "HLM", `Résidence` != "Maison des Gardes",`Résidence` != "TOTAL") %>%
  group_by(`Résidence`,`Année de gestion`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

#Standardiser le nom des résidence
logementdispo <- standardiser_residence(logementdispo)

#Ajout colonne `Nombre logement`
logement <- logement %>%
  left_join(
    logementdispo %>% select(`Résidence`, `Année de gestion`, `Nombre logement`),
    by = c("Résidence", "Année de gestion")
  )

logement <- logement %>%
  left_join(
    Test %>% select(`Résidence`, `Année de gestion`, `Demandes renouvellement`,
                    `Renouvellement confirmé`, `Taux acceptation`),
    by = c("Résidence", "Année de gestion")
  )


#2021-----
logement2021 <- read_xlsx("C:/Users/jeremie.dupont/Desktop/Stage/Logement/SUIVI LOGEMENT TOUR 2021-2022 – Jérémie.xlsx")

logement2021 <- logement2021 %>%
  fill(`Résidence`, .direction = "down") 

logement2021 <- logement2021 %>%
  filter(!is.na(`Résidence`)) %>% 
  filter(`Résidence` != "HLM", `Résidence` != "Maison des Gardes",`Résidence` != "TOTAL") %>%
  group_by(`Résidence`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

#Standardiser le nom des résidence
logement2021 <- standardiser_residence(logement2021)

# Trier les noms des résidences
logement_sorted <- sort(unique(logement$Résidence))
logement2021_sorted <- sort(unique(logement2021$Résidence))

diff_logement <- setdiff(logement_sorted, logement2021_sorted)
print("Présents dans logement mais pas dans logement2021 :")
print(diff_logement)

diff_logement2021 <- setdiff(logement2021_sorted, logement_sorted)
print("Présents dans logement2021 mais pas dans logement :")
print(diff_logement2021)

#2022-----
logement2022 <- read_xlsx("C:/Users/jeremie.dupont/Desktop/Stage/Logement/RESULTATS TOUR LOGEMENT 2022-2023 – Jérémie.xlsx")

logement2022 <- logement2022 %>%
  fill(`Résidence`, .direction = "down") 

logement2022 <- logement2022 %>%
  filter(!is.na(`Résidence`)) %>% 
  filter(`Résidence` != "Jean Zay", `Résidence` != "TOTAL") %>%
  group_by(`Résidence`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

logement2022 <- standardiser_residence(logement2022)
# Trier les noms des résidences 
logement2022_sorted <- sort(unique(logement2022$Résidence))

# Afficher les noms présents dans logement mais pas dans logement2022
diff_logement2 <- setdiff(logement_sorted, logement2022_sorted)
print("Présents dans logement mais pas dans logement2022 :")
print(diff_logement2)

# Afficher les noms présents dans logement2021 mais pas dans logement
diff_logement2022 <- setdiff(logement2022_sorted, logement_sorted)
print("Présents dans logement2022 mais pas dans logement :")
print(diff_logement2022)

#2023-----
logement2023 <- read_xlsx("C:/Users/jeremie.dupont/Desktop/Stage/Logement/RESULTATS TOUR LOGEMENT 2023-2024 – Jérémie.xlsx")

logement2023 <- logement2023 %>%
  fill(`Résidence`, .direction = "down") 

logement2023 <- logement2023 %>%
  filter(!is.na(`Résidence`)) %>% 
  filter(`Résidence` != "Jean Zay", `Résidence` != "TOTAL",`Résidence` != "Robespierre",`Résidence` != "Résidence Gaston Adriensence") %>%
  group_by(`Résidence`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

logement2023 <- standardiser_residence(logement2023)

# Trier les noms des résidences de chaque dataframe
logement2023_sorted <- sort(unique(logement2023$Résidence))

# Afficher les noms présents dans logement mais pas dans logement2022
diff_logement3 <- setdiff(logement_sorted, logement2023_sorted)
print("Présents dans logement mais pas dans logement2023 :")
print(diff_logement3)

# Afficher les noms présents dans logement2021 mais pas dans logement
diff_logement2023 <- setdiff(logement2023_sorted, logement_sorted)
print("Présents dans logement2022 mais pas dans logement :")
print(diff_logement2023)
#2024-----
logement2024 <- read_xlsx("C:/Users/jeremie.dupont/Desktop/Stage/Logement/RESULTATS TOUR LOGEMENT 2024-2025 – Jérémie.xlsx")

logement2024 <- logement2024 %>%
  fill(`Résidence`, .direction = "down") 

logement2024 <- logement2024 %>%
  filter(!is.na(`Résidence`)) %>% 
  filter(`Résidence` != "Résidence Jean Zay", `Résidence` != "TOTAL",`Résidence` != "Robespierre") %>%
  group_by(`Résidence`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))

logement2024 <- standardiser_residence(logement2024)

# Trier les noms des résidences de chaque dataframe
logement2024_sorted <- sort(unique(logement2024$Résidence))

# Afficher les noms présents dans logement mais pas dans logement2022
diff_logement4 <- setdiff(logement_sorted, logement2024_sorted)
print("Présents dans logement mais pas dans logement2024 :")
print(diff_logement4)

# Afficher les noms présents dans logement2021 mais pas dans logement
diff_logement2024 <- setdiff(logement2023_sorted, logement_sorted)
print("Présents dans logement2024 mais pas dans logement :")
print(diff_logement2024)


#Stocker les années dans une liste----
logement_par_annee <- list(
  "2022" = logement2022,
  "2023" = logement2023,
  "2024" = logement2024
)

################################################
########      Regroupement Fichier      ########
################################################
#TEST 2021
logement <- logement %>%
  # Filtrer uniquement les données pour l'année 2021
  filter(`Année de gestion` == 2021) %>%
  # Faire une jointure avec uniquement les colonnes nécessaires de logement2021
  left_join(
    logement2021 %>% select(`Résidence`, `TOUR 21-22`,`PhaseC`),
    by = "Résidence"
  ) %>%
  # Mettre à jour la colonne "Nombre de places"
  mutate(`Places Tour 1` = ifelse(!is.na(`TOUR 21-22`), `TOUR 21-22`, `Places Tour 1`)) %>%
  mutate(`Places phase complémentaire` = ifelse(!is.na(`PhaseC`), `PhaseC`,`Places phase complémentaire`)) %>% 
  mutate(`Places Total`=`Places Tour 1`) %>%
  # Supprimer la colonne temporaire "nb logement dispo"
  select(-`TOUR 21-22`,-`PhaseC`) %>%
  # Réintégrer les autres années
  bind_rows(filter(logement, `Année de gestion` != 2021))

#Test 2022
logement <- traiter_toutes_les_annees(logement, logement_par_annee, c(2022,2023,2024))

################################################
########    Enregistrement Fichier      ########
################################################
# Chemin
output_path <- "C:/Users/jeremie.dupont/Desktop/Stage/Logement/BDD Suivi des demandes logement1.xlsx"

# Enregistrer le dataframe modifié dans un fichier Excel
write_xlsx(logement, output_path)

# Message de confirmation
cat("Le fichier modifié a été enregistré avec succès à l'emplacement suivant :\n", output_path)

places1 <- logement %>% 
  filter(!is.na(`Année de gestion`), !is.na(`Secteur`), !is.na(`Places Total`)) %>% 
  distinct(`Année de gestion`, `Résidence`, .keep_all = TRUE) %>%  # Conserver une seule ligne par résidence et année
  group_by(`Année de gestion`) %>%  
  summarise(
    Total_Places_Uniques = sum(`Places Total`),
    .groups = "drop"
  )

print(places1)

demandes1 <- logement %>% 
  filter(!is.na(`Année de gestion`), !is.na(`Secteur`), !is.na(`INE`)) %>% 
  distinct(`Année de gestion`, `Secteur`, `INE`) %>%  # Conserver une seule ligne par résidence et année
  group_by(`Année de gestion`) %>%  
  summarise(
    Demande_unique = n(),
    .groups = "drop"
  )
 
print(demandes1)

logement %>% 
  filter(`Année de gestion` == 2023) %>% 
  count(`Résidence`, `Places Total`) %>% 
  arrange(desc(n))

#--------- Logement dispo 

# Chemin
output_path1 <- "C:/Users/jeremie.dupont/Desktop/Stage/Logement/Logement_dispo.xlsx"

# Enregistrer le dataframe modifié dans un fichier Excel
write_xlsx(logementdispo, output_path1)

# Message de confirmation
cat("Le fichier modifié a été enregistré avec succès à l'emplacement suivant :\n", output_path1)

