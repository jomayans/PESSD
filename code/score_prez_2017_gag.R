library(readxl)
library(dplyr)
library(haven)
library(lubridate)
library(tidyr)
library(readODS)


df2017 <- read_ods("/Users/augustajomayans/Documents/pessd/data/presidentielles/Presidentielle_2017_Resultats_Circonscription_T1_clean.ods")
df_final2017 <- read.csv("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2017/candidats_2017.csv", header = TRUE, sep = ",")

# Créer score_gagnabilite pour chaque parti dans chaque circonscription à partir de leurs résultats aux présidentielles
# Ajouter score_gagnabilite à chaque candidat aux législatives

df2017 <- df2017 %>%
  rename(
    SOC = HAMON_exp,
    LR = FILLON_exp,
    FI = MÉLENCHON_exp,
    REM = MACRON_exp,
    FN = `LE PEN_exp`,
    DLF = `DUPONT-AIGNAN_exp`
  )


# Liste des partis
candidats <- c("SOC", "LR", "FI", "REM", "FN", "DLF")


# Passer en format long (une ligne = une paire circo x parti)
df_long <- df2017 %>%
  pivot_longer(cols = all_of(candidats),
               names_to = "parti",
               values_to = "score_presidentielle")

# Calculer la gagnabilité relative
df_long <- df_long %>%
  group_by(CodeCirco) %>%
  mutate(max_score_circo = max(score_presidentielle, na.rm = TRUE),
         score_gagnabilite = score_presidentielle / max_score_circo) %>%
  ungroup()

# Repasser en format large pour score_gagnabilite
df_gagnabilite <- df_long %>%
  select(`CodeCirco`, parti, score_gagnabilite) %>%
  pivot_wider(names_from = parti,
              values_from = score_gagnabilite,
              names_glue = "{parti}_gagnabilite")

# Définir la date de référence 
date_reference <- ymd("2017-06-11")

# Convertir la date de naissance en format date
df_final2017 <- df_final2017 %>%
  mutate(date_naissance_candidat = ymd(date_naissance_candidat))

# Calculer l'âge
df_final2017 <- df_final2017 %>%
  mutate(age = round(as.numeric(date_reference - date_naissance_candidat) / 365.25))

df_final2017 <- df_final2017 %>%
  mutate(sortant = case_when(
    sortant == "Oui" ~ 1,
    sortant == "Non" ~ 0,
    TRUE ~ NA_real_  # Pour gérer les valeurs manquantes ou inattendues
  ))


# Principaux partis
df_final2017 <- df_final2017 %>%
  filter(nuance_candidat %in% c("SOC", "LR", "FI", "REM", "FN", "DLF"))

df_final2017 <- df_final2017 %>%
  mutate(code_departement = case_when(
    is.na(code_departement) & departement == "GUADELOUPE" ~ "ZA",
    is.na(code_departement) & departement == "MARTINIQUE" ~ "ZB",
    is.na(code_departement) & departement == "GUYANE" ~ "ZC",
    is.na(code_departement) & departement == "LA REUNION" ~ "ZD",
    is.na(code_departement) & departement == "MAYOTTE" ~ "ZM",
    is.na(code_departement) & departement == "NOUVELLE-CALEDONIE" ~ "ZN",
    is.na(code_departement) & departement == "POLYNESIE FRANCAISE" ~ "ZP",
    is.na(code_departement) & departement == "SAINT-PIERRE-ET-MIQUELON" ~ "ZS",
    is.na(code_departement) & departement == "WALLIS ET FUTUNA" ~ "ZW",
    is.na(code_departement) & departement == "SAINT-MARTIN/SAINT-BARTHELEMY" ~ "ZX",
    is.na(code_departement) & departement == "FRANCAIS ETABLIS HORS DE FRANCE" ~ "ZZ",
    is.na(code_departement) & departement == "CORSE-DU-SUD" ~ "2A",
    is.na(code_departement) & departement == "HAUTE-CORSE" ~ "2B",
    TRUE ~ as.character(code_departement)  # On garde les autres valeurs existantes
  ))

# Formatter la circonscription
df_final2017$code_circonscription_str <- sprintf("%02d", df_final2017$code_circonscription)

# Concaténer code_departement avec code_circonscription_str
df_final2017$CodeCirco <- paste0(df_final2017$code_departement, df_final2017$code_circonscription_str)

# Préparer les colonnes à merger
# Liste des colonnes parti_score dans df_gagnabilite
parti_scores <- c("SOC_gagnabilite", 
                  "LR_gagnabilite", "FI_gagnabilite", 
                  "REM_gagnabilite", "FN_gagnabilite", "DLF_gagnabilite")

# Merge des deux dataframes
df_final2017 <- df_final2017 %>%
  left_join(
    df_gagnabilite %>% 
      select(CodeCirco, all_of(parti_scores)),
    by = "CodeCirco"
  )

# Fonction pour attribuer score_gagnabilite
attribuer_score_fav <- function(row) {
  nuance <- row[["nuance_candidat"]]
  
  if (nuance %in% c("SOC", "LR", "FI", "REM", "FN", "DLF")) {
    score_col <- paste0(nuance, "_gagnabilite")
    if (score_col %in% names(row)) {
      return(row[[score_col]])
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# Appliquer la fonction pour créer la colonne score_gagnabilite
df_final2017 <- df_final2017 %>%
  rowwise() %>%
  mutate(score_gagnabilite = attribuer_score_fav(cur_data())) %>%
  ungroup()

df_final2017 <- df_final2017 %>%
  filter(!is.na(score_gagnabilite))

df_final2017 <- df_final2017 %>%
  mutate(dummy_value = 1) %>% 
  pivot_wider(names_from = profession_new, 
              values_from = dummy_value, 
              values_fill = 0)

# Save dataframe as do
chemin_fichier <- "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2017/candidats_2017_gag.dta"
write_dta(df_final2017, chemin_fichier)

