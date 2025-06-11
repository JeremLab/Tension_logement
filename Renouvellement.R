logement_unique <- logement %>%
  distinct(`Résidence`, `Secteur`)

Test <- read_xlsx("C:/Users/jeremie.dupont/Desktop/Stage/Logement/Renouvellement.xlsx") %>%
  select(`INE`, `Résidence`, `Année de gestion`, `Statut de la demande`) %>%
  filter(`Résidence` != "HLM Conventionnés", `Résidence` != "Maison des Gardes", `Résidence` != "TOTAL") %>%
  filter(!is.na(`Année de gestion`)) %>%
  mutate(`Année de gestion` = as.numeric(`Année de gestion`)) %>%
  left_join(logement_unique, by = "Résidence") %>%
  group_by(`Résidence`, `Année de gestion`, `Secteur`) %>%
  summarise(
    `Demandes renouvellement` = n(),
    `Renouvellement confirmé` = sum(`Statut de la demande` == "Confirmée", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Taux acceptation` = round(`Renouvellement confirmé` / `Demandes renouvellement` * 100, 2)
  )

#Test.
donnees_par_annee <- donnees_modele %>%
  group_by(`Contingent (Libellé)`) %>%
  summarise(nb_renouvellements = sum(nb_renouvellements), .groups = "drop")

print(donnees_par_annee)
