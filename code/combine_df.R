library(haven)
library(dplyr)

cand2012_fav <- read_dta("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2012/candidats_2012_fav.dta")
cand2017_fav <- read_dta("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2017/candidats_2017_fav.dta")
cand2022_fav <- read_dta("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2022/candidats_2022_fav.dta")
cand2024_fav <- read_dta("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2024/candidats_2024_fav.dta")

cand2012_gag <- read_dta("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2012/candidats_2012_gag.dta")
cand2017_gag <- read_dta("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2017/candidats_2017_gag.dta")
cand2022_gag <- read_dta("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2022/candidats_2022_gag.dta")
cand2024_gag <- read_dta("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2024/candidats_2024_gag.dta")

# Ce code combine tous les dataframe année par année, pour créer le dataset pour les régressions

# Liste des variables communes pour faire la jointure
vars_communes <- c("législature", "code_departement", "departement",
                   "libelle_circonscription", "sexe_candidat", "nom_candidat", "prenom_candidat",
                   "date_naissance_candidat", "nuance_candidat", "sortant", "selection_second_tour",
                   "age", "CodeCirco",
                   "Inactifs_précaires", "Prof_int", "Autres_professions", "Prof_lib_et_scient",
                   "Commerçants_artisans", "Cadres_et_PI_sup", "Employes", "Ouvriers", "Agriculteurs")

# Fonction pour faire la fusion et la sélection
fusionner_donnees <- function(df_fav, df_gag) {
  left_join(df_fav, df_gag %>% select(all_of(vars_communes), score_gagnabilite),
            by = vars_communes) %>%
    select(all_of(vars_communes), score_favorabilite, score_gagnabilite)
}

cand2012 <- fusionner_donnees(cand2012_fav, cand2012_gag)
cand2017 <- fusionner_donnees(cand2017_fav, cand2017_gag)
cand2022 <- fusionner_donnees(cand2022_fav, cand2022_gag)
cand2024 <- fusionner_donnees(cand2024_fav, cand2024_gag)

# Liste des variables à conserver
vars_to_keep <- c("législature", "code_departement", "departement",
                  "libelle_circonscription", "sexe_candidat", "nom_candidat", "prenom_candidat",
                  "date_naissance_candidat", "nuance_candidat", "sortant", "selection_second_tour",
                  "age", "CodeCirco", "score_favorabilite", "score_gagnabilite",
                  "Inactifs_précaires", "Prof_int", "Autres_professions", "Prof_lib_et_scient",
                  "Commerçants_artisans", "Cadres_et_PI_sup", "Employes", "Ouvriers", "Agriculteurs")

cand_combined <- bind_rows(
  select(cand2012, any_of(vars_to_keep)),
  select(cand2017, any_of(vars_to_keep)),
  select(cand2022, any_of(vars_to_keep)),
  select(cand2024, any_of(vars_to_keep))
)

cand_combined <- cand_combined %>%
  mutate(mp_id = group_indices(., nom_candidat, prenom_candidat))

# Création variables parti_pol et mouvance
cand_combined <- cand_combined %>%
  mutate(
    parti_pol = case_when(
      nuance_candidat %in% c("FN", "RN") ~ "RN",
      nuance_candidat %in% c("UMP", "LR") ~ "LR",
      nuance_candidat %in% c("REM", "ENS") ~ "ENS",
      nuance_candidat %in% c("NUP", "UG") ~ "NUP",
      TRUE ~ nuance_candidat
    )
  )

cand_combined <- cand_combined %>%
  mutate(
    mouvance = case_when(
      nuance_candidat %in% c("FG", "SOC", "FI", "NUP", "UG") ~ "gauche",
      nuance_candidat %in% c("REC", "RN", "FN", "DLF", "LR", "UMP") ~ "droite",
      TRUE ~ NA_character_
    )
  )

# Création de la variable circo_gagnable
cand_combined <- cand_combined %>%
  mutate(circo_gagnable = if_else(score_gagnabilite == 1, 1, 0))

# Save dataframe as do
chemin_fichier <- "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/candidats.dta"
write_dta(cand_combined, chemin_fichier)

# Save dataframe as csv
chemin_fichier <- "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/candidats.csv"
write.csv(cand_combined, chemin_fichier)

