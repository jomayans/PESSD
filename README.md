# Parité en politique : Évolution des candidatures féminines aux élections législatives et accès aux circonscriptions gagnables (2002-2024)

Joséphine MAYANS, Kenza MIOUSSET

L’analyse repose sur un ensemble de données publiques disponibles via l’explorateur de données de la plateforme data.gouv.fr, qui recense les candidatures et les résultats pour les deux tours des élections législatives et présidentielles françaises. Nous avons mobilisé les fichiers correspondants aux scrutins de 2002, 2007, 2012, 2017, 2022 et 2024. 

Un travail d’harmonisation a été nécessaire pour mettre en cohérence les informations issues des différentes années, en raison des variations de structure ou de nomenclature entre les fichiers.

## Codes score_prez_ANNEE_fav.R et score_prez_ANNEE_gag.R

### Étapes principales du script

#### Importation des données :
* Résultats des présidentielles 2022 par circonscription
Données des candidat·es aux législatives 2022
Construction du score de favorabilité :
Pour chaque parti, un score de favorabilité est calculé dans chaque circonscription à partir du score obtenu au premier tour des présidentielles, normalisé entre 0 et 1.
Une agrégation est réalisée pour la coalition NUPES (somme de LFI, PS, PCF, ECO).
Fusion avec les données législatives :
Le score de favorabilité du parti est attribué à chaque candidat·e, en fonction de sa circonscription et de son étiquette politique.
Le fichier final contient donc, pour chaque candidat·e :
Son âge
Son parti
Son statut de sortant
Le score de favorabilité de son parti dans sa circonscription
Formatage des données :
Création d’identifiants de circonscription (CodeCirco)
Passage au format large pour les professions des candidat·es (variables binaires)
Export final en fichier .dta (format Stata) pour exploitation économétrique



Les codes score_prez_ANNEE_fav.R et score_prez_ANNEE_gag.R, permettent respectivement de créer les variables score_favorabilite et score_gagnabilite associés à chaque parti pour chaque circonscription, par rapport aux score de ces partis lors des élections présidentielles.
