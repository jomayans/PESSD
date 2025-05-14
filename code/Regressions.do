
*** MODELE DE REGRESSION LINEAIRE POUR LA GAGNABILITE/FAVORABILITE DES CIRCONSCRIPTION (Table 1)

use "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/candidats.dta", clear

global depvar score_favorabilite
global mainregressor sexe_candidat

gen age2 = age^2
encode nuance_candidat, generate(nuance_candidat_num)
encode CodeCirco, generate(CodeCirco_num)

global controls1 age age2
global controls2 sortant Agriculteurs Prof_lib_et_scient Cadres_et_PI_sup Inactifs_précaires Autres_professions Employes Ouvriers Commerçants_artisans Prof_int
global fe_nuance_candidat i.nuance_candidat_num
global fe_CodeCirco_num i.CodeCirco_num
global fe_législature i.législature


label var sexe_candidat "Femme"
label var age "Age"
label var age2 "Age au carré"
label var sortant "Député sortant"

* Régression 1 : Baseline
reg $depvar $mainregressor, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls ""
    estadd local political_experience_controls ""
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg1

* Régression 2 : Âge
reg $depvar $mainregressor $controls1, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls ""
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg2

* Régression 3 : Expérience politique
reg $depvar $mainregressor $controls1 $controls2, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg3

* Régression 4 : Effets fixes Nuance candidat
reghdfe $depvar $mainregressor $controls1 $controls2, absorb($fe_nuance_candidat) vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE "Oui"
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg4

* Régression 5 : Effets fixes CodeCirco_num
reghdfe $depvar $mainregressor $controls1 $controls2, absorb($fe_nuance_candidat $fe_CodeCirco_num $fe_législature) vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE "Oui"
    estadd local CodeCirco_num_FE "Oui"
    estadd local Legislature_FE "Oui"
    estimates store reg5

* Génération tableau LaTeX
esttab reg1 reg2 reg3 reg4 reg5 using "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/output/OLS_favorabilite.tex", ///
    title("Regression circonscription gagnable sur femme") ///
    label star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    drop(_cons) ///
    collabels(none) nomtitles ///
    stats(age_controls political_experience_controls nuance_candidat_FE CodeCirco_num_FE Legislature_FE ///
          separator1 obs r2, ///
        fmt(%9s %9s %9s %9s %9s %9s ///
            %9s %9.0fc %9.3f) ///
        labels("Âge" "Expérience" "Parti FE" "Circonscription FE" "Législature FE" ///
               "\midrule" "Observations" "Adj. R-squared")) ///
    replace style(tab) nonotes


	
** Comparaison partis politiques (Table 2)

* LR

use "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/candidats.dta", clear

keep if parti_pol == "LR"

global depvar score_favorabilite
global mainregressor sexe_candidat

gen age2 = age^2
encode CodeCirco, generate(CodeCirco_num)

global controls1 age age2
global controls2 sortant Agriculteurs Prof_lib_et_scient Cadres_et_PI_sup Inactifs_précaires Autres_professions Employes Ouvriers Commerçants_artisans Prof_int
global fe_CodeCirco_num i.CodeCirco_num
global fe_législature i.législature

label var sexe_candidat "Femme"
label var age "Age"
label var age2 "Age au carré"
label var sortant "Député sortant"

* Régression 1 : Baseline
reg $depvar $mainregressor, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls ""
    estadd local political_experience_controls ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg1

* Régression 2 : + âge
reg $depvar $mainregressor $controls1, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg2

* Régression 3 : + expérience politique
reg $depvar $mainregressor $controls1 $controls2, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg3

* Régression 4 (ex-régression 5) : FE circonscription et législature (pas de parti)
reghdfe $depvar $mainregressor $controls1 $controls2, absorb($fe_CodeCirco_num $fe_législature) vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local CodeCirco_num_FE "Oui"
    estadd local Legislature_FE "Oui"
    estimates store reg4

* Export vers LaTeX
esttab reg1 reg2 reg3 reg4 using ///
"/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/output/OLS_favorabilite_LR.tex", ///
    title("Régression circonscription favorable sur genre - LR") ///
    label star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    drop(_cons) ///
    collabels(none) nomtitles ///
    stats(age_controls political_experience_controls CodeCirco_num_FE Legislature_FE ///
          separator1 obs r2, ///
        fmt(%9s %9s %9s %9s %9s %9.0fc %9.3f) ///
        labels("Âge" "Expérience" "Circonscription FE" "Législature FE" ///
               "\midrule" "Observations" "Adj. R-squared")) ///
    replace style(tab) nonotes

	
* Mouvance politique

use "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/candidats.dta", clear

keep if mouvance == "droite"

global depvar score_favorabilite
global mainregressor sexe_candidat

gen age2 = age^2
encode nuance_candidat, generate(nuance_candidat_num)
encode CodeCirco, generate(CodeCirco_num)

global controls1 age age2
global controls2 sortant Agriculteurs Prof_lib_et_scient Cadres_et_PI_sup Inactifs_précaires Autres_professions Employes Ouvriers Commerçants_artisans Prof_int
global fe_nuance_candidat i.nuance_candidat_num
global fe_CodeCirco_num i.CodeCirco_num
global fe_législature i.législature


label var sexe_candidat "Femme"
label var age "Age"
label var age2 "Age au carré"
label var sortant "Député sortant"

* Régression 1 : Baseline
reg $depvar $mainregressor, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls ""
    estadd local political_experience_controls ""
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg1

* Régression 2 : Âge
reg $depvar $mainregressor $controls1, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls ""
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg2

* Régression 3 : Expérience politique
reg $depvar $mainregressor $controls1 $controls2, vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg3

* Régression 4 : Effets fixes Nuance candidat
reghdfe $depvar $mainregressor $controls1 $controls2, absorb($fe_nuance_candidat) vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE "Oui"
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg4

* Régression 5 : Effets fixes CodeCirco_num
reghdfe $depvar $mainregressor $controls1 $controls2, absorb($fe_nuance_candidat $fe_CodeCirco_num $fe_législature) vce(robust)
    estadd scalar obs=e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE "Oui"
    estadd local CodeCirco_num_FE "Oui"
    estadd local Legislature_FE "Oui"
    estimates store reg5

* Génération tableau LaTeX
esttab reg1 reg2 reg3 reg4 reg5 using "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/output/OLS_favorabilite_droite.tex", ///
    title("Regression circonscription gagnable sur femme - droite") ///
    label star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    drop(_cons) ///
    collabels(none) nomtitles ///
    stats(age_controls political_experience_controls nuance_candidat_FE CodeCirco_num_FE Legislature_FE ///
          separator1 obs r2, ///
        fmt(%9s %9s %9s %9s %9s %9s ///
            %9s %9.0fc %9.3f) ///
        labels("Âge" "Expérience" "Parti FE" "Circonscription FE" "Législature FE" ///
               "\midrule" "Observations" "Adj. R-squared")) ///
    replace style(tab) nonotes	
	

	

*** MODELE DE REGRESSION LINEAIRE POUR LA GAGNABILITE/FAVORABILITE DES CIRCONSCRIPTION - AVEC INTERACTIONS genre x année (Annexe)
	
use "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/candidats.dta", clear

global depvar score_favorabilite

gen age2 = age^2
encode nuance_candidat, generate(nuance_candidat_num)
encode CodeCirco, generate(CodeCirco_num)

global controls1 age age2
global controls2 sortant Agriculteurs Prof_lib_et_scient Cadres_et_PI_sup Inactifs_précaires Autres_professions Employes Ouvriers Commerçants_artisans Prof_int
global fe_nuance_candidat i.nuance_candidat_num
global fe_CodeCirco_num i.CodeCirco_num
global fe_législature i.législature

label var sexe_candidat "Femme"
label var age "Age"
label var age2 "Age au carré"
label var sortant "Député sortant"

* Régression 1 : Interaction Femme × Législature
reg $depvar i.sexe_candidat##i.législature, vce(robust)
    estadd scalar obs = e(N)
    estadd local age_controls ""
    estadd local political_experience_controls ""
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg1

* Régression 2 : Ajout de l'âge
reg $depvar i.sexe_candidat##i.législature $controls1, vce(robust)
    estadd scalar obs = e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls ""
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg2

* Régression 3 : + expérience politique
reg $depvar i.sexe_candidat##i.législature $controls1 $controls2, vce(robust)
    estadd scalar obs = e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE ""
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg3

* Régression 4 : Effets fixes parti
reghdfe $depvar i.sexe_candidat##i.législature $controls1 $controls2, absorb($fe_nuance_candidat) vce(robust)
    estadd scalar obs = e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE "Oui"
    estadd local CodeCirco_num_FE ""
    estadd local Legislature_FE ""
    estimates store reg4

* Régression 5 : Effets fixes parti + circonscription (pas de FE législature)
reghdfe $depvar i.sexe_candidat##i.législature $controls1 $controls2, absorb($fe_nuance_candidat $fe_CodeCirco_num) vce(robust)
    estadd scalar obs = e(N)
    estadd local age_controls "Oui"
    estadd local political_experience_controls "Oui"
    estadd local nuance_candidat_FE "Oui"
    estadd local CodeCirco_num_FE "Oui"
    estadd local Legislature_FE ""   // <- FE législature retirés ici
    estimates store reg5

* Export vers LaTeX
esttab reg1 reg2 reg3 reg4 reg5 using ///
"/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/output/OLS_favorabilite_interactions.tex", ///
    title("Effet du genre sur la favorabilité des circonscriptions (interactions Femme × législature)") ///
    label star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    drop(_cons) ///
    collabels(none) nomtitles ///
    stats(age_controls political_experience_controls nuance_candidat_FE CodeCirco_num_FE Legislature_FE ///
          separator1 obs r2, ///
        fmt(%9s %9s %9s %9s %9s %9s ///
            %9s %9.0fc %9.4f) ///
        labels("Âge" "Expérience" "Parti FE" "Circo FE" "Législature FE" ///
               "\midrule" "Observations" "R-squared")) ///
    replace style(tab) nonotes


	
*** MODELE LOGIT pour circonscription gagnable (Section 5.3)

use "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/candidats.dta", clear

gen age2 = age^2
encode CodeCirco, gen(CodeCirco_num)
encode nuance_candidat, gen(nuance_candidat_num)

* Régression logistique avec effets fixes de circonscription et de législature
logit circo_gagnable ///
    i.sexe_candidat ///
    age age2 ///
    sortant Agriculteurs Prof_lib_et_scient Cadres_et_PI_sup Inactifs_précaires Autres_professions ///
    Employes Ouvriers Commerçants_artisans Prof_int ///
    i.CodeCirco_num i.législature, ///
    vce(robust)

* Odds ratio
logit, or

* Export LaTeX
eststo logitmodel

esttab logitmodel using ///
"/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/output/logit_gagnable.tex", ///
    title("Modèle logit : circonscription gagnable (1 si parti en tête à la présidentielle)") ///
    label star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    drop(_cons) ///
    nomtitles collabels(none) ///
    replace style(tab) nonotes

	
*** MODELE LOGIT (hors députés sortants)

use "/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/data/legislatives/candidats.dta", clear

* Garder uniquement les candidats qui ne sont pas députés sortants
keep if sortant == 0

* Préparer les variables
gen age2 = age^2
encode CodeCirco, gen(CodeCirco_num)
encode nuance_candidat, gen(nuance_candidat_num)

* Régression logistique avec effets fixes de circonscription et de législature
logit circo_gagnable ///
    i.sexe_candidat ///
    age age2 ///
    Agriculteurs Prof_lib_et_scient Cadres_et_PI_sup Inactifs_précaires Autres_professions ///
    Employes Ouvriers Commerçants_artisans Prof_int ///
    i.CodeCirco_num i.législature, ///
    vce(robust)

* Odds ratio
logit, or

* Export LaTeX
eststo logitmodel

esttab logitmodel using ///
"/Users/augustajomayans/Library/CloudStorage/GoogleDrive-josephine.mayans@gmail.com/Mon Drive/pessd/output/logit_gagnable_nonsortants.tex", ///
    title("Modèle logit (hors sortants) : circonscription gagnable (1 si parti en tête à la présidentielle)") ///
    label star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    drop(_cons) ///
    nomtitles collabels(none) ///
    replace style(tab) nonotes

	
	
