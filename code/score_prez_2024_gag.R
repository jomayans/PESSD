library(readxl)
library(dplyr)
library(haven)
library(lubridate)
library(tidyr)


df2022 <- read_excel("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/presidentielles/resultats-par-niveau-cirlg-t1-france-entiere.xlsx", sheet = 1)
df_final2024 <- read.csv("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2024/candidats_2024.csv", header = TRUE, sep = ",")

# Créer score_gagnabilite pour chaque parti dans chaque circonscription à partir de leurs résultats aux présidentielles
# Ajouter score_gagnabilite à chaque candidat aux législatives

df2022 <- df2022 %>%
  rename(
    LO = `% Voix/Exp`,
    PCF = `...33`,
    ENS = `...40`,
    Resistons = `...47`,
    RN = `...54`,
    REC = `...61`,
    FI = `...68`,
    SOC = `...75`,
    ECO = `...82`,
    LR = `...89`,
    NPA = `...96`,
    DLF = `...103`
  ) %>%
  select(
    `Code du département`,
    `Code de la circonscription`,
    LO, PCF, ENS, Resistons, RN, REC, FI, SOC, ECO, LR, NPA, DLF
  )

df2022$UG <- df2022$PCF + df2022$FI + df2022$SOC + df2022$ECO

candidats <- c("LO", "PCF", "ENS", "Resistons", "RN", "REC",
               "FI", "SOC", "ECO", "LR", "NPA", "DLF", "UG")

# Passer en format long (une ligne = une paire circo x parti)
df_long <- df2022 %>%
  pivot_longer(cols = all_of(candidats),
               names_to = "parti",
               values_to = "score_presidentielle")

# Passer en format long (une ligne = une paire circo x parti)
# Créer un identifiant unique pour la circo
df_long <- df_long %>%
  unite("CodeCirco", `Code du département`, `Code de la circonscription`, remove = FALSE)

# Calculer la gagnabilité relative
df_long <- df_long %>%
  group_by(CodeCirco) %>%
  mutate(max_score_circo = max(score_presidentielle, na.rm = TRUE),
         score_gagnabilite = score_presidentielle / max_score_circo) %>%
  ungroup()

# Repasser en format large pour score_gagnabilite
df_gagnabilite <- df_long %>%
  select(`Code du département`, `Code de la circonscription`, parti, score_gagnabilite) %>%
  pivot_wider(names_from = parti,
              values_from = score_gagnabilite,
              names_glue = "{parti}_gagnabilite")

# Définir la date de référence
date_reference <- ymd("2024-06-30")

# Convertir la date de naissance en format date
df_final2024 <- df_final2024 %>%
  mutate(date_naissance_candidat = dmy(date_naissance_candidat))

# Calculer l'âge
df_final2024 <- df_final2024 %>%
  mutate(age = round(as.numeric(date_reference - date_naissance_candidat) / 365.25))

df_final2024 <- df_final2024 %>%
  mutate(sortant = case_when(
    sortant == "OUI" ~ 1,
    sortant == "NON" ~ 0,
    TRUE ~ NA_real_ 
  ))

# Pour l'instant je ne m'occupe que des principaux partis
df_final2024 <- df_final2024 %>%
  filter(nuance_candidat %in% c("ENS", "LR", "RN", "REC", "ECO", "SOC", "FI", "UG"))

# Uniformiser le format de `code_circonscription`
df_final2024 <- df_final2024 %>%
  mutate(code_circonscription = sprintf("%02d", as.numeric(code_circonscription)))

# Uniformiser également `Code de la circonscription` dans df_gagnabilite (précaution)
df_gagnabilite <- df_gagnabilite %>%
  mutate(`Code de la circonscription` = sprintf("%02d", as.numeric(`Code de la circonscription`)))

# Préparer les colonnes à merger
# Liste des colonnes parti_score dans df_gagnabilite
parti_scores <- c("ENS_gagnabilite", 
                  "LR_gagnabilite", "RN_gagnabilite", 
                  "REC_gagnabilite", "ECO_gagnabilite", "SOC_gagnabilite", "FI_gagnabilite", "UG_gagnabilite")

# Merge des deux dataframes
df_final2024 <- df_final2024 %>%
  left_join(
    df_gagnabilite %>% 
      select(`Code du département`, `Code de la circonscription`, all_of(parti_scores)),
    by = c("code_departement" = "Code du département",
           "code_circonscription" = "Code de la circonscription")
  )

# Fonction pour attribuer score_gagnabilite
attribuer_score_fav <- function(row) {
  nuance <- row[["nuance_candidat"]]
  
  if (nuance %in% c("ENS", "LR", "RN", "REC", "ECO", "SOC", "FI", "UG")) {
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
df_final2024 <- df_final2024 %>%
  rowwise() %>%
  mutate(score_gagnabilite = attribuer_score_fav(cur_data())) %>%
  ungroup()

df_final2024 <- df_final2024 %>%
  mutate(circonscription = group_indices(., code_departement, code_circonscription))

df_final2024$code_departement <- as.character(df_final2024$code_departement)

# Concaténer code_departement avec code_circonscription_str
df_final2024$CodeCirco <- paste0(df_final2024$code_departement, df_final2024$code_circonscription)

df_final2024$CodeCirco <- sub("^0", "", df_final2024$CodeCirco)

df_final2024 <- df_final2024 %>%
  mutate(dummy_value = 1) %>% 
  pivot_wider(names_from = profession_new, 
              values_from = dummy_value, 
              values_fill = 0)

# Save dataframe as do
chemin_fichier <- "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2024/candidats_2024_gag.dta"
write_dta(df_final2024, chemin_fichier)
