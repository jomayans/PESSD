library(readxl)
library(dplyr)
library(haven)
library(lubridate)
library(tidyr)

df2022 <- read_excel("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/presidentielles/resultats-par-niveau-cirlg-t1-france-entiere.xlsx", sheet = 1)
df_final2022 <- read.csv("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2022/candidats_2022_2.csv", header = TRUE, sep = ",")

# Créer score_favorabilite pour chaque parti dans chaque circonscription à partir de leurs résultats aux présidentielles
# Ajouter score_favorabilite à chaque candidat aux législatives

df2022 <- df2022 %>%
  rename(
    LO = `% Voix/Exp`,
    PCF = `...33`,
    ENS = `...40`,
    Resistons = `...47`,
    RN = `...54`,
    REC = `...61`,
    LFI = `...68`,
    PS = `...75`,
    ECO = `...82`,
    LR = `...89`,
    NPA = `...96`,
    DLF = `...103`
  ) %>%
  select(
    `Code du département`,
    `Code de la circonscription`,
    LO, PCF, ENS, Resistons, RN, REC, LFI, PS, ECO, LR, NPA, DLF
  )


# Créer la variable de score agrégé NUP (NUPES)
df2022$NUP <- df2022$PCF + df2022$LFI + df2022$PS + df2022$ECO

# Liste des partis
candidats <- c("LO", "PCF", "ENS", "Resistons", "RN", "REC",
               "LFI", "PS", "ECO", "LR", "NPA", "DLF", "NUP")


# Passer en format long (une ligne = une paire circo x parti)
df_long <- df2022 %>%
  pivot_longer(cols = c("LO", "PCF", "ENS", "Resistons", "RN", "REC", "LFI", 
                        "PS", "ECO", "LR", "NPA", "DLF", "NUP"),
               names_to = "parti",
               values_to = "score_presidentielle")

# Normaliser les scores de chaque parti
df_long <- df_long %>%
  group_by(parti) %>%
  mutate(score_favorabilite = (score_presidentielle - min(score_presidentielle, na.rm = TRUE)) /
           (max(score_presidentielle, na.rm = TRUE) - min(score_presidentielle, na.rm = TRUE))) %>%
  ungroup()

# Repasser en wide
df_favorabilite <- df_long %>%
  select(`Code du département`, `Code de la circonscription`, parti, score_favorabilite) %>%
  pivot_wider(names_from = parti,
              values_from = score_favorabilite,
              names_glue = "{parti}_favorabilite")


# Définir la date de référence (12 juin 2022)
date_reference <- ymd("2022-06-12")

# Convertir la date de naissance en format date
df_final2022 <- df_final2022 %>%
  mutate(date_naissance_candidat = ymd(date_naissance_candidat))

# Calculer l'âge
df_final2022 <- df_final2022 %>%
  mutate(age = round(as.numeric(date_reference - date_naissance_candidat) / 365.25))


df_final2022 <- df_final2022 %>%
  mutate(sortant = case_when(
    sortant == "Oui" ~ 1,
    sortant == "Non" ~ 0,
    TRUE ~ NA_real_ 
  ))

# Principaux partis
df_final2022 <- df_final2022 %>%
  filter(nuance_candidat %in% c("ENS", "LR", "RN", "REC", "ECO", "NUP"))

# Uniformiser le format de `code_circonscription`
df_final2022 <- df_final2022 %>%
  mutate(code_circonscription = sprintf("%02d", as.numeric(code_circonscription)))

# Uniformiser également `Code de la circonscription` dans df_favorabilite (précaution)
df_favorabilite <- df_favorabilite %>%
  mutate(`Code de la circonscription` = sprintf("%02d", as.numeric(`Code de la circonscription`)))

# Préparer les colonnes à merger
# Liste des colonnes parti_score dans df_favorabilite
parti_scores <- c("ENS_favorabilite", 
                  "RN_favorabilite", "REC_favorabilite", 
                  "ECO_favorabilite", "NUP_favorabilite", "LR_favorabilite")

# Merge des deux dataframes
df_final2022 <- df_final2022 %>%
  left_join(
    df_favorabilite %>% 
      select(`Code du département`, `Code de la circonscription`, all_of(parti_scores)),
    by = c("code_departement" = "Code du département",
           "code_circonscription" = "Code de la circonscription")
  )

# Fonction pour attribuer score_favorabilite
attribuer_score_fav <- function(row) {
  nuance <- row[["nuance_candidat"]]
  
  if (nuance %in% c("ENS", "LR", "RN", "REC", "ECO", "NUP")) {
    score_col <- paste0(nuance, "_favorabilite")
    if (score_col %in% names(row)) {
      return(row[[score_col]])
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# Appliquer la fonction pour créer la colonne score_favorabilite
df_final2022 <- df_final2022 %>%
  rowwise() %>%
  mutate(score_favorabilite = attribuer_score_fav(cur_data())) %>%
  ungroup()


df_final2022 <- df_final2022 %>%
  mutate(circonscription = group_indices(., code_departement, code_circonscription))

df_final2022$code_departement <- as.character(df_final2022$code_departement)

# Concaténer code_departement avec code_circonscription_str
df_final2022$CodeCirco <- paste0(df_final2022$code_departement, df_final2022$code_circonscription)

df_final2022$CodeCirco <- sub("^0", "", df_final2022$CodeCirco)

df_final2022 <- df_final2022 %>%
  mutate(dummy_value = 1) %>% 
  pivot_wider(names_from = profession_new, 
              values_from = dummy_value, 
              values_fill = 0)

# Save dataframe as do
chemin_fichier <- "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2022/candidats_2022_fav.dta"
write_dta(df_final2022, chemin_fichier)
