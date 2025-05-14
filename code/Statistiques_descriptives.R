library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(tools)
library(stringi)
library(data.table)
library(tidyr)
library(haven)
library(ggrepel)
library(patchwork) 
library(forcats)



#Chargements fichiers
candidats_2002 <- read.csv("/Users/kenzamiousset/Documents/3A/Projet /candidats_2002.csv")
candidats_2007 <- read.csv("/Users/kenzamiousset/Documents/3A/Projet /candidats_2007.csv")
candidats_2012 <- read.csv("/Users/kenzamiousset/Documents/3A/Projet /candidats_2012.csv")
candidats_2017 <- read.csv("/Users/kenzamiousset/Documents/3A/Projet /candidats_2017.csv")
candidats_2022 <- read.csv("/Users/kenzamiousset/Documents/3A/Projet /candidats_2022.csv")
candidats_2024 <- read.csv("/Users/kenzamiousset/Documents/3A/Projet /candidats_2024.csv")


candidats_2002$code_departement <- as.character(candidats_2002$code_departement)
candidats_2007$code_departement <- as.character(candidats_2007$code_departement)
candidats_2012$code_departement <- as.character(candidats_2012$code_departement)
candidats_2017$code_departement <- as.character(candidats_2017$code_departement)
candidats_2022$code_departement <- as.character(candidats_2022$code_departement)
candidats_2024$code_departement <- as.character(candidats_2024$code_departement)

# Fusion
candidats <- bind_rows(candidats_2002, candidats_2007, candidats_2012,
                       candidats_2017, candidats_2022, candidats_2024)



###################################################################
#Part de femmes parmi les candidates, les qualifiées pour le 2nd tour et les députés
####################################################################

######Part de femmes candidates au 1er tour
part_femmes <- candidats %>%
  group_by(législature) %>%
  summarise(
    total_candidats = n(),
    femmes = sum(sexe_candidat == 1, na.rm = TRUE),
    part_femmes = round(100 * femmes / total_candidats, 1)
  ) %>%
  arrange(législature)

part_femmes <- part_femmes %>%
  mutate(année = case_when(
    législature == 12 ~ "2002",
    législature == 13 ~ "2007",
    législature == 14 ~ "2012",
    législature == 15 ~ "2017",
    législature == 16 ~ "2022",
    législature == 17 ~ "2024"
  ))


# Graphique en courbe
graph_part_femmes <- ggplot(part_femmes, aes(x = année, y = part_femmes, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = paste0(part_femmes, "%")), 
            vjust = -1, size = 4, color = "black") +
  labs(
    x = "Année", 
    y = "Part des femmes (%)", 
    title = "Évolution de la part des femmes candidates au premier tour"
  ) +
  theme_minimal(base_size = 14) +
  ylim(35, 50)  

print(graph_part_femmes)
ggsave(filename = "/Users/kenzamiousset/Documents/3A/Projet /résultats/part_1ert.png", 
       plot = graph_part_femmes,
       width = 8, height = 5, dpi = 300)



######Part de femmes qualifiées au 2nd tour

# Calcul de la part de femmes parmi les qualifié·es au second tour
part_femmes_chez_qualifies <- candidats %>%
  filter(selection_second_tour == 1) %>%
  mutate(sexe = if_else(sexe_candidat == 1, "Femme", "Homme")) %>%
  group_by(législature) %>%
  summarise(
    total_qualifies = n(),
    femmes_qualifies = sum(sexe == "Femme"),
    part_femmes = round(100 * femmes_qualifies / total_qualifies, 1)
  ) %>%
  ungroup()

print(part_femmes_chez_qualifies)

part_femmes_chez_qualifies <- part_femmes_chez_qualifies %>%
  mutate(année = case_when(
    législature == 12 ~ "2002",
    législature == 13 ~ "2007",
    législature == 14 ~ "2012",
    législature == 15 ~ "2017",
    législature == 16 ~ "2022",
    législature == 17 ~ "2024"
  ))


# Graphique en courbe de la part de femmes parmi les qualifié·es
graph_part_femmes_chez_qualifies <- ggplot(part_femmes_chez_qualifies, aes(x = année, y = part_femmes, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = paste0(part_femmes, "%")), 
            vjust = -1, size = 4, color = "black") +
  labs(
    x = "Année",
    y = "Part de femmes parmi les qualifié·es (%)",
    title = "Évolution de la part de femmes qualifiées au second tour"
  ) +
  theme_minimal(base_size = 14) +
  ylim(20, 50)  

print(graph_part_femmes_chez_qualifies)

ggsave(filename = "/Users/kenzamiousset/Documents/3A/Projet /résultats/part_2t.png", 
       plot = graph_part_femmes_chez_qualifies,
       width = 8, height = 5, dpi = 300)




######Candidates, qualifiées et députés 
df1 <- part_femmes %>%
  select(année, part = part_femmes) %>%
  mutate(etape = "Part de femmes candidates au 1er tour")

df2 <- part_femmes_chez_qualifies %>%
  select(année, part = part_femmes) %>%
  mutate(etape = "Part de femmes qualifiées pour le 2nd tour")

df3 <- data.frame(
  année = c("2002", "2007", "2012", "2017", "2022", "2024"),
  part  = c(12.3, 18.5, 26.9, 38.8, 37.3, 36.1),
  etape = "Part de femmes élues députées"
)

# Fusion 
df_combined <- bind_rows(df1, df2, df3) %>%
  mutate(
    année = factor(année, levels = c("2002", "2007", "2012", "2017", "2022", "2024")),
    etape = factor(etape, levels = c(
      "Part de femmes candidates au 1er tour",
      "Part de femmes qualifiées pour le 2nd tour",
      "Part de femmes élues députées"
    )),
    vjust = case_when(
      etape == "Part de femmes élues députées" & année == "2017" ~ 1.5,
      etape == "Part de femmes qualifiées pour le 2nd tour" & année %in% c("2022", "2024") ~ 1.5,
      TRUE ~ -1
    )
  )

# Graphique
part_femmes_combined <- ggplot(df_combined, aes(x = année, y = part, color = etape, group = etape)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_text(aes(label = paste0(part, "%"), vjust = vjust), size = 3, show.legend = FALSE) +
  scale_color_manual(values = c(
    "Part de femmes candidates au 1er tour" = "blue",
    "Part de femmes qualifiées pour le 2nd tour" = "#e61ddb",
    "Part de femmes élues députées" = "#ff7f00"
  )) +
  scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, 5), labels = function(x) paste0(x, "%")) +
  labs(
    x = "Année",
    y = "Part de femmes (%)",
    color = "Étape"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )

part_femmes_combined


ggsave(
  filename = "/Users/kenzamiousset/Documents/3A/Projet /Résultats/part_femmes_combined.png", 
  plot = part_femmes_combined,
  width = 10, height = 5, dpi = 300
)


#############################################################
#Part de femmes parmi les candidates par formation politique
############################################################


# Harmonisation des noms de partis et ajout de l'année
candidats_graph <- candidats %>%
  mutate(
    parti_harmonise = case_when(
      nuance_candidat %in% c("UMP") ~ "LR",
      nuance_candidat %in% c("RN") ~ "FN",
      nuance_candidat %in% c("UG") ~ "NUP",
      nuance_candidat %in% c("REM") ~ "ENS",
      législature == 17 & nuance_candidat %in% c("FI", "SOC") ~ "NUP",
      TRUE ~ nuance_candidat
    ),
    annee = case_when(
      législature == 12 ~ "2002",
      législature == 13 ~ "2007",
      législature == 14 ~ "2012",
      législature == 15 ~ "2017",
      législature == 16 ~ "2022",
      législature == 17 ~ "2024",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(annee))

# Sélection des partis à afficher
partis_selectionnes <- c("FN", "ECO", "SOC", "LR", "FI", "NUP", "REC", "ENS")

# Calcul de la part de femmes
part_femmes_par_parti <- candidats_graph %>%
  filter(parti_harmonise %in% partis_selectionnes) %>%
  group_by(annee, parti_harmonise) %>%
  summarise(
    nb_total = n(),
    nb_femmes = sum(sexe_candidat == 1, na.rm = TRUE),
    part_femmes = round(100 * nb_femmes / nb_total, 1),
    .groups = "drop"
  ) %>%
  mutate(
    etiquette = paste0(part_femmes, "%")
  )

# Ordre politique des partis
partis_ordonne <- c("FI", "NUP", "SOC", "ECO", "ENS", "LR", "REC", "FN")
part_femmes_par_parti$parti_harmonise <- factor(part_femmes_par_parti$parti_harmonise, levels = partis_ordonne)

# Couleurs par parti
couleurs_partis <- c(
  "FI" = "#C80064",
  "NUP" = "#A349A4",
  "SOC" = "#FF6F61",
  "ECO" = "#77AB43",
  "ENS" = "#FDB813",
  "LR" = "#0055A4",
  "REC" = "#939393",
  "FN" = "#002654"
)

# Etiquettes conditionnelles
part_femmes_par_parti <- part_femmes_par_parti %>%
  mutate(
    label_afficher = case_when(
      (annee == "2022" & parti_harmonise %in% c("NUP", "FN", "LR", "ECO")) ~ TRUE,
      (annee == "2024" & parti_harmonise %in% c("FN", "ENS", "LR", "ECO", "REC")) ~ TRUE,
      (annee == "2012" & parti_harmonise %in% c("FN", "SOC", "LR", "ECO")) ~ TRUE,
      TRUE ~ FALSE
    ),
    vjust = case_when(
      (annee == "2022" & parti_harmonise == "ECO") ~ 2,
      (annee == "2024" & parti_harmonise %in% c("REC", "ECO")) ~ 2,
      (annee == "2012" & parti_harmonise == "ECO") ~ 2,
      TRUE ~ -1
    )
  )


part_femmes_courbe <- ggplot(part_femmes_par_parti, aes(x = as.factor(annee), y = part_femmes, color = parti_harmonise, group = parti_harmonise)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_text(
    data = part_femmes_par_parti %>% filter(label_afficher),
    aes(label = etiquette, vjust = vjust),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = couleurs_partis, name = "Parti") +
  scale_y_continuous(limits = c(19, 52), breaks = seq(20, 50, 5), labels = function(x) paste0(x, "%")) +
  labs(
    x = "Année",
    y = "Part de femmes (%)",
    color = "Parti"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 12),
    legend.position = "right"
  )

ggsave(
  filename = "/Users/kenzamiousset/Documents/3A/Projet /Résultats/part_femmes_par_parti.png", 
  plot = part_femmes_courbe,
  width = 10, height = 7, dpi = 300
)




##########################################
#Score de favorabilité et de gagnabilité
##########################################

candidats <- read.csv("/Users/kenzamiousset/Documents/3A/Projet /Résultats/candidats.csv")

candidats <- candidats %>%
  mutate(
    annee = case_when(
      législature == 12 ~ "2002",
      législature == 13 ~ "2007",
      législature == 14 ~ "2012",
      législature == 15 ~ "2017",
      législature == 16 ~ "2022",
      législature == 17 ~ "2024"
    ),
    sexe = if_else(sexe_candidat == 1, "Femme", "Homme")
  )

# Création d'une variable combinée année + sexe 
candidats <- candidats_jo %>%
  mutate(annee_sexe = paste(annee, "-", sexe))

# Filtrer les observations valides 
candidats_valide <- candidats %>%
  filter(!is.na(score_favorabilite), !is.na(sexe), !is.na(annee_sexe))

score_fav <- ggplot(candidats_valide, aes(x = annee_sexe, y = score_favorabilite, fill = sexe)) +
  stat_boxplot(geom = "errorbar", width = 0.3, color = "black") +  # barres horizontales min/max
  geom_boxplot(outlier.shape = NA, color = "black") +
  scale_fill_manual(values = c("Femme" = "#e78ac3", "Homme" = "#8da0cb")) +
  labs(
    x = NULL,
    y = "Score_favorabilite",
    fill = "Sexe"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

score_fav

ggsave(filename = "/Users/kenzamiousset/Documents/3A/Projet /résultats/score_fav.png", 
       plot = score_fav,
       width = 8, height = 5, dpi = 300)


#####Score favorabilité par sexe (années confondues)

# Filtrer les données valides
candidats_valide <- candidats_jo %>%
  filter(!is.na(score_favorabilite), !is.na(sexe), !is.na(annee_sexe))

# Tracer un boxplot global par sexe
score_fav_full <- ggplot(candidats_valide, aes(x = sexe, y = score_favorabilite, fill = sexe)) +
  stat_boxplot(geom = "errorbar", width = 0.3, color = "black") +  # barres horizontales min/max
  geom_boxplot(outlier.shape = NA, color = "black") +              # boîte sans outliers
  scale_fill_manual(values = c("Femme" = "#e78ac3", "Homme" = "#8da0cb")) +
  labs(
    title = "Distribution du score_favorabilite selon le sexe",
    x = "Sexe",
    y = "Score score_favorabilite",
    fill = "Sexe"
  ) +
  theme_minimal(base_size = 13)

score_fav_full

ggsave(filename = "/Users/kenzamiousset/Documents/3A/Projet /résultats/score_fav_full.png", 
       plot = score_fav_full,
       width = 8, height = 5, dpi = 300)



######Score de gagnabilité 

# Filtrer les données valides
candidats_valide <- candidats_jo_ %>%
  filter(!is.na(score_gagnabilite), !is.na(sexe), !is.na(annee_sexe))

# Tracer le boxplot
score_gagn <- ggplot(candidats_valide, aes(x = annee_sexe, y = score_gagnabilite, fill = sexe)) +
  stat_boxplot(geom = "errorbar", width = 0.3, color = "black") +
  geom_boxplot(color = "black", outlier.shape = NA) +
  scale_fill_manual(values = c("Femme" = "#e78ac3", "Homme" = "#8da0cb")) +
  labs(
    title = "Distribution de 'score_gagnabilite' par sexe et année",
    x = NULL,
    y = "Score score_gagnabilite",
    fill = "Sexe"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

score_gagn

ggsave(filename = "/Users/kenzamiousset/Documents/3A/Projet /résultats/score_gagn.png", 
       plot = score_gagn,
       width = 8, height = 5, dpi = 300)




#######################################################
#Distribution de l'âge par sexe (annexe)
#######################################################


###AGE
# Mise en forme des dates de naissance selon la législature
candidats <- candidats %>%
  mutate(
    date_naissance_candidat = case_when(
      législature %in% c(12, 13, 14, 17) ~ as.Date(date_naissance_candidat, format = "%d/%m/%Y"),
      législature %in% c(15, 16) ~ as.Date(date_naissance_candidat, format = "%Y-%m-%d"),
      TRUE ~ as.Date(NA)
    )
  )

# Dates des élections
dates_election <- c(
  "12" = "2002-06-09",
  "13" = "2007-06-10",
  "14" = "2012-06-10",
  "15" = "2017-06-11",
  "16" = "2022-06-12",
  "17" = "2024-06-30"
)

# Ajout de l'âge, tranche d'âge, et année
candidats <- candidats %>%
  mutate(
    date_election = as.Date(dates_election[as.character(législature)]),
    age = as.integer(floor((as.numeric(date_election - date_naissance_candidat)) / 365.25)),
    tranche_age = cut(
      age,
      breaks = c(18, 30, 40, 50, 60, 70, Inf),
      labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      right = FALSE
    ),
    tranche_age = factor(tranche_age, levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+")),
    annee = case_when(
      législature == 12 ~ "2002",
      législature == 13 ~ "2007",
      législature == 14 ~ "2012",
      législature == 15 ~ "2017",
      législature == 16 ~ "2022",
      législature == 17 ~ "2024"
    )
  )


# Répartition par sexe dans chaque tranche d’âge
candidats <- candidats %>%
  mutate(
    sexe = if_else(sexe_candidat == 1, "Femme", "Homme")
  )

part_sexe_age <- candidats %>%
  filter(!is.na(tranche_age), !is.na(sexe)) %>%
  group_by(annee, tranche_age, sexe) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(annee, tranche_age) %>%
  mutate(proportion = round(n / sum(n) * 100, 1)) %>%
  ungroup()


candidats_age_valide <- candidats %>%
  filter(!is.na(age), age >= 18, age <= 100)

#graphique
age_moustache <- ggplot(candidats_age_valide, aes(x = sexe, y = age, fill = sexe)) +
  stat_boxplot(geom = "errorbar", width = 0.3, color = "black") +
  geom_boxplot(outlier.shape = NA, color = "black") +
  facet_wrap(~ annee, ncol = 3) +
  scale_fill_manual(values = c("Femme" = "#e78ac3", "Homme" = "#8da0cb")) +
  labs(
    x = "Sexe",
    y = "Âge",
    fill = "Sexe"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )

age_moustache

ggsave(
  filename = "/Users/kenzamiousset/Documents/3A/Projet /Résultats/moustaches_age.png", 
  plot = age_moustache,
  width = 8, height = 7, dpi = 300
)

# Calcul des quartiles par législature et par sexe
quartiles_age <- candidats %>%
  filter(!is.na(age), !is.na(sexe)) %>%
  group_by(législature, sexe) %>%
  summarise(
    Q1 = quantile(age, 0.25),
    Q2 = quantile(age, 0.50),
    Q3 = quantile(age, 0.75),
    .groups = "drop"
  )
print(quartiles_age)



##########################################
#Professions (annexe)
##########################################

#TABLEAU
candidats <- candidats %>%
  mutate(
    annee = case_when(
      législature == 12 ~ 2002,
      législature == 13 ~ 2007,
      législature == 14 ~ 2012,
      législature == 15 ~ 2017,
      législature == 16 ~ 2022,
      législature == 17 ~ 2024
    ),
    sexe = if_else(sexe_candidat == 1, "Femme", "Homme")
  )

# Filtrer les professions valides
candidats_filtrés <- candidats %>%
  filter(!is.na(profession_new), profession_new != "Police_militaires")

# Calcul des parts par profession, année et sexe
table_combined <- candidats_filtrés %>%
  count(annee, sexe, profession_new) %>%
  group_by(annee, sexe) %>%
  mutate(part = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(colonne = paste0(annee, "_", sexe)) %>%
  select(profession_new, colonne, part) %>%
  pivot_wider(names_from = colonne, values_from = part)

# Affichage du tableau final
print(table_combined)


write.csv(
  table_combined,
  file = "/Users/kenzamiousset/Documents/3A/Projet /Résultats/profs_sexe.csv",
  row.names = FALSE
)


#Camemberts 2012/2017

# Création de la variable année
candidats <- candidats %>%
  mutate(annee = case_when(
    législature == 12 ~ 2002,
    législature == 13 ~ 2007,
    législature == 14 ~ 2012,
    législature == 15 ~ 2017,
    législature == 16 ~ 2022,
    législature == 17 ~ 2024
  ))

# Fonction pour créer les données de camembert
prep_data_camembert <- function(df, year) {
  df %>%
    filter(annee == year, !is.na(profession_new), profession_new != "Police_militaires") %>%
    count(profession_new) %>%
    mutate(
      part = n / sum(n),
      label = paste0(round(100 * part, 1), "%")
    )
}

# Données pour 2012 et 2017
data_2012_total <- prep_data_camembert(candidats, 2012)
data_2017_total <- prep_data_camembert(candidats, 2017)


couleurs_profs <- c(
  "Agriculteurs" = "#ffd92f",
  "Autres_professions" = "#fc8d62",
  "Cadres_et_PI_sup" = "#8da0cb",
  "Commerçants_artisans" = "#e78ac3",
  "Employes" = "#a6d854",
  "Inactifs_précaires" = "#b3b3b3",
  "Ouvriers" = "#e5c494",
  "Prof_int" = "#66c2a5",
  "Prof_lib_et_scient" = "#a1d99b"
)


# Camembert 2012
camembert_2012 <- ggplot(data_2012_total, aes(x = "", y = part, fill = profession_new)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  coord_polar(theta = "y") +
  labs(title = "2012") +
  scale_fill_manual(values = couleurs_profs) +
  theme_void(base_size = 13) +
  theme(legend.position = "none")

# Camembert 2017
camembert_2017 <- ggplot(data_2017_total, aes(x = "", y = part, fill = profession_new)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  coord_polar(theta = "y") +
  labs(title = "2017") +
  scale_fill_manual(values = couleurs_profs) +
  theme_void(base_size = 13) +
  theme(legend.position = "bottom")

# Combinaison avec patchwork
prof_2012_2017 <- camembert_2012 + camembert_2017 + plot_layout(ncol = 2)

ggsave(
  filename = "/Users/kenzamiousset/Documents/3A/Projet /Résultats/prof_2012_2017.png", 
  plot = prof_2012_2017,
  width = 8, height = 7, dpi = 300
)






