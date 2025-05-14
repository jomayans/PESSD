library(readxl)
library(dplyr)
library(haven)
library(lubridate)
library(tidyr)

candidats_prez <- read_excel("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/presidentielles/2012/candidats_prez.xlsx", sheet = 1)
general_prez <- read_excel("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/presidentielles/2012/general_prez.xlsx", sheet = 1)

# D'abord, on met en forme le df avec les scores des partis aux présidentielles

general_prez <- general_prez %>%
  filter(id_election != "2012_pres_t2")

general_prez <- general_prez[, c("Code du d√©partement", "Code du b.vote", "Code de la commune", "Code de la circonscription")]

general_prez <- general_prez %>%
  rename(
    code_departement = `Code du d√©partement`,
    code_circonscription = `Code de la circonscription`
  )

general_prez$code_departement <- as.character(general_prez$code_departement)

# Formatter la circonscription avec deux chiffres
general_prez$code_circonscription <- sprintf("%02d", general_prez$code_circonscription)

# Concaténer code_departement avec code_circonscription_str
general_prez$CodeCirco <- paste0(general_prez$code_departement, general_prez$code_circonscription)

candidats_prez <- candidats_prez %>%
  filter(id_election != "2012_pres_t2")

candidats_prez <- candidats_prez[, c("Code du d√©partement", "Code du b.vote", "Code de la commune", "Voix", "Nom")]

candidats_prez <- candidats_prez %>%
  rename(
    code_departement = `Code du d√©partement`
  )

candidats_prez <- merge(candidats_prez, 
                        general_prez[, c("code_departement", "Code du b.vote", "Code de la commune", "CodeCirco")],
                        by.x = c("code_departement", "Code du b.vote", "Code de la commune"),
                        by.y = c("code_departement", "Code du b.vote", "Code de la commune"),
                        all.x = TRUE)

resultat <- aggregate(Voix ~ CodeCirco + Nom, data = candidats_prez, FUN = sum)

resultat_large <- resultat %>%
  pivot_wider(
    names_from = Nom,
    values_from = Voix,
    values_fill = 0
  )

resultat_pourcent <- resultat_large %>%
  # Calculer le total des voix par ligne
  mutate(total_voix = rowSums(across(-CodeCirco)), .after = CodeCirco) %>%
  # Convertir chaque colonne en pourcentage
  mutate(across(-c(CodeCirco, total_voix), ~ round(./total_voix * 100, 2))) %>%
  select(-total_voix)



# Créer score_favorabilite pour chaque parti dans chaque circonscription à partir de leurs résultats aux présidentielles
# Ajouter score_favorabilite à chaque candidat aux législatives

df2012 <- resultat_pourcent
df_final2012 <- read.csv("/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2012/candidats_2012.csv", header = TRUE, sep = ",")


df2012 <- df2012 %>%
  rename(
    CEN = BAYROU,
    SOC = HOLLANDE,
    ECO = JOLY,
    FN = "LE PEN",
    FG = MELENCHON,
    UMP = SARKOZY
  )

# Liste des partis
candidats <- c("CEN", "SOC", "ECO", "FN", "FG", "UMP")

# Passer en format long (une ligne = une paire circo x parti)
df_long <- df2012 %>%
  pivot_longer(cols = c("CEN", "SOC", "ECO", "FN", "FG", "UMP"),
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
  select(`CodeCirco`, parti, score_favorabilite) %>%
  pivot_wider(names_from = parti,
              values_from = score_favorabilite,
              names_glue = "{parti}_favorabilite")


# Définir la date de référence 
date_reference <- ymd("2012-06-10")

df_final2012 <- df_final2012 %>%
  mutate(date_naissance_candidat = dmy(date_naissance_candidat))

# Calculer l'âge
df_final2012 <- df_final2012 %>%
  mutate(age = round(as.numeric(date_reference - date_naissance_candidat) / 365.25))

df_final2012 <- df_final2012 %>%
  mutate(sortant = case_when(
    sortant == "OUI" ~ 1,
    sortant == "NON" ~ 0,
    TRUE ~ NA_real_  # Pour gérer les valeurs manquantes ou inattendues
  ))


# Principaux partis
df_final2012 <- df_final2012 %>%
  filter(nuance_candidat %in% c("CEN", "SOC", "ECO", "FN", "FG", "UMP"))


df_final2012$code_departement <- as.character(df_final2012$code_departement)

# Formatter la circonscription avec deux chiffres
df_final2012$code_circonscription_str <- sprintf("%02d", df_final2012$code_circonscription)

# Concaténer code_departement avec code_circonscription_str
df_final2012$CodeCirco <- paste0(df_final2012$code_departement, df_final2012$code_circonscription_str)

df_final2012$CodeCirco <- sub("^0", "", df_final2012$CodeCirco)

# Préparer les colonnes à merger
# Liste des colonnes parti_score dans df_favorabilite
parti_scores <- c("CEN_favorabilite", 
                  "SOC_favorabilite", "ECO_favorabilite", 
                  "FN_favorabilite", "FG_favorabilite", "UMP_favorabilite")

# Merge des deux dataframes
df_final2012 <- df_final2012 %>%
  left_join(
    df_favorabilite %>% 
      select(CodeCirco, all_of(parti_scores)),
    by = "CodeCirco"
  )

# Fonction pour attribuer score_favorabilite
attribuer_score_fav <- function(row) {
  nuance <- row[["nuance_candidat"]]
  
  if (nuance %in% c("CEN", "SOC", "ECO", "FN", "FG", "UMP")) {
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
df_final2012 <- df_final2012 %>%
  rowwise() %>%
  mutate(score_favorabilite = attribuer_score_fav(cur_data())) %>%
  ungroup()

df_final2012 <- df_final2012 %>%
  filter(!is.na(score_favorabilite))

df_final2012 <- df_final2012 %>%
  mutate(dummy_value = 1) %>% 
  pivot_wider(names_from = profession_new, 
              values_from = dummy_value, 
              values_fill = 0)


# Save dataframe as do
chemin_fichier <- "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/2012/candidats_2012_fav.dta"
write_dta(df_final2012, chemin_fichier)
