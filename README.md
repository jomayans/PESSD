# Parité en politique : Évolution des candidatures féminines aux élections législatives et accès aux circonscriptions gagnables (2002-2024)

Joséphine MAYANS, Kenza MIOUSSET

L’analyse repose sur un ensemble de données publiques disponibles via l’explorateur de données de la plateforme data.gouv.fr, qui recense les candidatures et les résultats pour les deux tours des élections législatives et présidentielles françaises. Nous avons mobilisé les fichiers correspondants aux scrutins de 2002, 2007, 2012, 2017, 2022 et 2024. 

Un travail d’harmonisation a été nécessaire pour mettre en cohérence les informations issues des différentes années, en raison des variations de structure ou de nomenclature entre les fichiers.

## Codes score_prez_ANNEE_fav.R et score_prez_ANNEE_gag.R
Chaque dataset (législatives et présidentielles) a une structure différente selon l'année, c'est pourquoi nous avons un code par année.

### Étapes principales du script

#### Importation des données :
* Résultats des présidentielles par circonscription
* Données des candidats aux législatives

#### Construction du score de favorabilité :
Pour chaque parti, un score de favorabilité (ou de gagnabilité) est calculé dans chaque circonscription à partir du score obtenu au premier tour des présidentielles, normalisé entre 0 et 1.

#### Fusion avec les données législatives :
Le score de favorabilité du parti est attribué à chaque candidat, en fonction de sa circonscription et de son étiquette politique.
Le fichier final annuel contient donc, pour chaque candidat :
* Son âge
* Son parti
* Son statut de sortant
* Le score de favorabilité de son parti dans sa circonscription

## Code combine_df.R
Ce code combine les datasets annuels créés précédemment.
On obtient candidats.dta, le dataset qui nous servira pour les régressions.

## Code Statistiques_Descriptives.R

#### Chargement et fusion des bases de candidats pour toutes les élections entre 2002 et 2024
Utilise les .csv nettoyés candidats_2002, candidats_2007, candidats_2012, candidats_2017, candidats_2022 et candidats_2024

#### Calculs et visualisations de :
* La part de femmes à chaque étape électorale
* Les écarts entre partis (graphique comparatif 2002–2024)
* Les scores de favorabilité et de gagnabilité, avec une attention au genre
* Les différences d'âge et de répartition professionnelle

## Code Regressions.do
Ce code Stata permet d’estimer les régressions suivantes :
* Table 1 : Régressions OLS du score de favorabilité sur le genre du candidat (avec contrôles et effets fixes progressifs).
* Table 2 : Régressions OLS restreintes à certains partis (ex. LR) ou mouvances (ex. droite).
* Section 5.3 : Régressions logit sur la probabilité d’obtenir une circonscription gagnable, avec et sans députés sortants.
* Annexe : Régressions avec interactions genre × législature pour tester la variation temporelle de l’effet.

Chaque spécification est exportée au format LaTeX pour être directement intégrée dans l’article.



