
# Partie 3 du Rapport Statapp: analyse factorielle et classification des atteintes dans la dimension spatiale

# ACP-CAH

# Analyse factorielle:

library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(Factoshiny)

library(corrplot)

# Stratégie 1: on réalise une ACP en prenant les communes comme individus et en choisissant comme variables actives
# le nombre d'infractions (I) et le nombre de victimes (V) rapportés au nombre d'habitants de la commune.

# A) On réalise une ACP à partir du tibble "del2016_2021_com_IV" généré dans le script "Partie2_rapport_stat_des.R":
# Il s'agit d'un tableau de données avec en lignes les communes et en colonnes le nombre d'infractions (I) et
# de victimes pour 1000 habitants.

# calcul du nombre d'infractions I et du nombre de victimes V pour 1000 habitants au niveau de la commune:
# on se restreint aux atteintes pour lesquelles les commune de I et de V sont renseignées (on perd environ 60 000 atteintes
# sur 6,5 millions)

# on reprend le code de la partie 2 et on le complète pour les besoins de l'ACP:
del2016_2021_dt_com_I <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
  Nb_I = sum(compteur, na.rm = TRUE),
  Nb_I_cambr = sum(compteur*(classe2 == "Cambriolages de logement"), na.rm = TRUE),
  Nb_I_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_I_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_I_destr_degrad = sum(compteur*(classe2 == "Destructions et dégradations"), na.rm = TRUE),
  Nb_I_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
  Nb_I_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE),
  Nb_I_vols_armes = sum(compteur*(classe2 == "Vols avec armes"), na.rm = TRUE),
  Nb_I_vols_acces_vehic = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
  Nb_I_vols_ds_vehic = sum(compteur*(classe2 == "Vols dans les véhicules"), na.rm = TRUE),
  Nb_I_vols_de_vehic = sum(compteur*(classe2 == "Vols de véhicules"), na.rm = TRUE),
  Nb_I_vols_sansviol = sum(compteur*(classe2 == "Vols sans violence contre des personnes"), na.rm = TRUE),
  Nb_I_vols_violants_sansarme = sum(compteur*(classe2 == "Vols violents sans arme"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]

del2016_2021_dt_com_V <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
  Nb_V = sum(compteur, na.rm = TRUE),
  Nb_V_cambr = sum(compteur*(classe2 == "Cambriolages de logement"), na.rm = TRUE),
  Nb_V_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_V_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_V_destr_degrad = sum(compteur*(classe2 == "Destructions et dégradations"), na.rm = TRUE),
  Nb_V_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
  Nb_V_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE),
  Nb_V_vols_armes = sum(compteur*(classe2 == "Vols avec armes"), na.rm = TRUE),
  Nb_V_vols_acces_vehic = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
  Nb_V_vols_ds_vehic = sum(compteur*(classe2 == "Vols dans les véhicules"), na.rm = TRUE),
  Nb_V_vols_de_vehic = sum(compteur*(classe2 == "Vols de véhicules"), na.rm = TRUE),
  Nb_V_vols_sansviol = sum(compteur*(classe2 == "Vols sans violence contre des personnes"), na.rm = TRUE),
  Nb_V_vols_violants_sansarme = sum(compteur*(classe2 == "Vols violents sans arme"), na.rm = TRUE)),
  by = .(cog_com_22_vict)]

# on apparie ces 2 fichiers avec le fichier "communes_zonages" reliant chaque commune aux différents zonages
# administratifs et d'étude de l'Insee:

# warning: on fait une jointure à gauche de communes_zonages_dt, de sorte à avoir l'ensemble des communes
# répertoriées par l'Insee récemment (population de référence):

del2016_2021_dt_com_I <- 
  merge(x = communes_zonages_dt,
        y = del2016_2021_dt_com_I,
        by.x = "CODGEO",
        by.y = "cog_com_22_inf",
        all.x = TRUE)
del2016_2021_dt_com_IV <- 
  merge(x = del2016_2021_dt_com_I,
        y = del2016_2021_dt_com_V,
        by.x = "CODGEO",
        by.y = "cog_com_22_vict",
        all.x = TRUE)
rm(del2016_2021_dt_com_I)
rm(del2016_2021_dt_com_V)

# on apparie ce fichier avec les infos socio-éco et démo sur les communes:

del2016_2021_dt_com_IV <- 
  merge(x = del2016_2021_dt_com_IV,
        y = infos_communes_dt,
        by.x = "CODGEO",
        by.y = "CODGEO",
        all.x = TRUE)

names(del2016_2021_dt_com_IV)

del2016_2021_com_IV <- as_tibble(del2016_2021_dt_com_IV)

# On calcule les 26 ratios "nombre d'infractions pour 1000 habitants" au niveau communal:
# On se restreint aux communes ayant au moins une infraction et une victime sur la période 2016-2021
data_com_IV_acp <- del2016_2021_com_IV %>% 
                                       filter(P19_POP>1 & Nb_I>0 & Nb_V>0) %>%
                                       mutate(I=Nb_I/P19_POP*1000,V=Nb_V/P19_POP*1000,
                                              I_cambr=Nb_I_cambr/P19_POP*1000,V_cambr=Nb_V_cambr/P19_POP*1000,
                                              I_blessures_famil=Nb_I_blessures_famil/P19_POP*1000,V_blessures_famil=Nb_V_blessures_famil/P19_POP*1000,
                                              I_blessures_horsfamil=Nb_I_blessures_horsfamil/P19_POP*1000,V_blessures_horsfamil=Nb_V_blessures_horsfamil/P19_POP*1000,
                                              I_destr_degrad=Nb_I_destr_degrad/P19_POP*1000,V_destr_degrad=Nb_V_destr_degrad/P19_POP*1000,
                                              I_homic=Nb_I_homic/P19_POP*1000,V_homic=Nb_V_homic/P19_POP*1000,
                                              I_viol_sex=Nb_I_viol_sex/P19_POP*1000,V_viol_sex=Nb_V_viol_sex/P19_POP*1000,
                                              I_vols_armes=Nb_I_vols_armes/P19_POP*1000,V_vols_armes=Nb_V_vols_armes/P19_POP*1000,
                                              I_vols_acces_vehic=Nb_I_vols_acces_vehic/P19_POP*1000,V_vols_acces_vehic=Nb_V_vols_acces_vehic/P19_POP*1000,
                                              I_vols_ds_vehic=Nb_I_vols_ds_vehic/P19_POP*1000,V_vols_ds_vehic=Nb_V_vols_ds_vehic/P19_POP*1000,
                                              I_vols_de_vehic=Nb_I_vols_de_vehic/P19_POP*1000,V_vols_de_vehic=Nb_V_vols_de_vehic/P19_POP*1000,
                                              I_vols_sansviol=Nb_I_vols_sansviol/P19_POP*1000,V_vols_sansviol=Nb_V_vols_sansviol/P19_POP*10000,
                                              I_vols_violants_sansarme=Nb_I_vols_violants_sansarme/P19_POP*1000,V_vols_violants_sansarme=Nb_V_vols_violants_sansarme/P19_POP*1000
                                             ) %>%
                                       select(CODGEO,I,V,I_cambr,V_cambr,I_blessures_famil,V_blessures_famil,
                                              I_blessures_horsfamil,V_blessures_horsfamil,I_destr_degrad,V_destr_degrad,
                                              I_homic,V_homic,I_viol_sex,V_viol_sex,I_vols_armes,V_vols_armes,
                                              I_vols_acces_vehic,V_vols_acces_vehic,I_vols_ds_vehic,V_vols_ds_vehic,
                                              I_vols_de_vehic,V_vols_de_vehic,I_vols_sansviol,V_vols_sansviol,
                                              I_vols_violants_sansarme,V_vols_violants_sansarme) %>%
                                      column_to_rownames(var="CODGEO")

summary(data_com_IV_acp)

# On réalise l'ACP sur ce tableau de données:

# res_acp_data_com_IV_acp <- PCA(data_com_IV_acp, scale.unit = TRUE, ncp = 5, graph = TRUE)
# summary(res_acp_data_com_IV_acp)

# IMPORTANT: R ne supporte pas cette ACP (trop de lignes dans le tableau, trop de valeurs nulles?...)

# B) On relance l'ACP sur un tableau plus réduit en termes de lignes:
# Pour ce faire, on impose des contraintes supplémentaires sur les communes retenues:
# on crée un jeu de contraintes de stricte positivité sur les nombres de I et de V pour tous
# les types d'atteintes:

data_com_IV_acp2 <- del2016_2021_com_IV %>% 
  filter(P19_POP>1 & Nb_I>0 & Nb_V>0 &
           Nb_I_cambr>0 & Nb_V_cambr>0 & Nb_I_blessures_famil>0 & Nb_V_blessures_famil>0 &
           Nb_I_blessures_horsfamil>0 & Nb_V_blessures_horsfamil>0 & Nb_I_destr_degrad>0 &
           Nb_V_destr_degrad>0 & Nb_I_homic>0 & Nb_V_homic>0 & Nb_I_viol_sex>0 & Nb_V_viol_sex>0 &
           Nb_I_vols_armes>0 & Nb_V_vols_armes>0 & Nb_I_vols_acces_vehic>0 & Nb_V_vols_acces_vehic>0 &
           Nb_I_vols_ds_vehic>0 & Nb_V_vols_ds_vehic>0 & Nb_I_vols_de_vehic>0 & Nb_V_vols_de_vehic>0 &
           Nb_I_vols_sansviol>0 & Nb_V_vols_sansviol>0 & Nb_I_vols_violants_sansarme>0 &
           Nb_V_vols_violants_sansarme>0) %>%
  mutate(I=Nb_I/P19_POP*1000,V=Nb_V/P19_POP*1000,
         I_cambr=Nb_I_cambr/P19_POP*1000,V_cambr=Nb_V_cambr/P19_POP*1000,
         I_blessures_famil=Nb_I_blessures_famil/P19_POP*1000,V_blessures_famil=Nb_V_blessures_famil/P19_POP*1000,
         I_blessures_horsfamil=Nb_I_blessures_horsfamil/P19_POP*1000,V_blessures_horsfamil=Nb_V_blessures_horsfamil/P19_POP*1000,
         I_destr_degrad=Nb_I_destr_degrad/P19_POP*1000,V_destr_degrad=Nb_V_destr_degrad/P19_POP*1000,
         I_homic=Nb_I_homic/P19_POP*1000,V_homic=Nb_V_homic/P19_POP*1000,
         I_viol_sex=Nb_I_viol_sex/P19_POP*1000,V_viol_sex=Nb_V_viol_sex/P19_POP*1000,
         I_vols_armes=Nb_I_vols_armes/P19_POP*1000,V_vols_armes=Nb_V_vols_armes/P19_POP*1000,
         I_vols_acces_vehic=Nb_I_vols_acces_vehic/P19_POP*1000,V_vols_acces_vehic=Nb_V_vols_acces_vehic/P19_POP*1000,
         I_vols_ds_vehic=Nb_I_vols_ds_vehic/P19_POP*1000,V_vols_ds_vehic=Nb_V_vols_ds_vehic/P19_POP*1000,
         I_vols_de_vehic=Nb_I_vols_de_vehic/P19_POP*1000,V_vols_de_vehic=Nb_V_vols_de_vehic/P19_POP*1000,
         I_vols_sansviol=Nb_I_vols_sansviol/P19_POP*1000,V_vols_sansviol=Nb_V_vols_sansviol/P19_POP*10000,
         I_vols_violants_sansarme=Nb_I_vols_violants_sansarme/P19_POP*1000,V_vols_violants_sansarme=Nb_V_vols_violants_sansarme/P19_POP*1000
  ) %>%
  select(CODGEO,LIBGEO.x,DEP.x,I,V,I_cambr,V_cambr,I_blessures_famil,V_blessures_famil,
         I_blessures_horsfamil,V_blessures_horsfamil,I_destr_degrad,V_destr_degrad,
         I_homic,V_homic,I_viol_sex,V_viol_sex,I_vols_armes,V_vols_armes,
         I_vols_acces_vehic,V_vols_acces_vehic,I_vols_ds_vehic,V_vols_ds_vehic,
         I_vols_de_vehic,V_vols_de_vehic,I_vols_sansviol,V_vols_sansviol,
         I_vols_violants_sansarme,V_vols_violants_sansarme) %>%
  column_to_rownames(var="CODGEO")
# Après filtrage, tableau de 1016 communes (au lieu des 34 638 initiales!)
summary(data_com_IV_acp2)

res_acp_data_com_IV_acp2 <- PCA(data_com_IV_acp2[3:28], scale.unit = TRUE, ncp = 5, graph = TRUE)
summary(res_acp_data_com_IV_acp2)

dimdesc(res_acp_data_com_IV_acp2)

plot(res_acp_data_com_IV_acp2,select="cos2 0.7")

#res = PCAshiny(data_com_IV_acp2[3:28])
# Cette ACP a bien tourné ;)

# Interprétation des résultats de l'ACP 
# L'ACP est normée (les variables actives ont été centrées-réduites)

# Le plan factoriel constitués des deux premiers axes expliquent 90,5% de l'inertie totale.
# -> on se limitera donc à l'interprétation des deux premiers axes factoriels.

# Interprétation du 1er axe factoriel: 81,3% de l'inertie totale
# Les 26 variables actives sont corrélées fortement et positivement au premier facteur.
# Etant ainsi liées à une même variable, elles sont toutes liées entre elles 
# (voir matrice de corrélations):
mcor <- cor(data_com_IV_acp2[3:28])
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
# On peut néanmoins remarquer que deux types d'atteintes sont moins corrélées avec les autres:
# les homicides et les vols violents avec armes.
# Il s'agit de deux atteintes parmi les plus violentes et qui recquièrent l'usage d'armes.
# Ce sont ces deux atteintes qui contribuent le moins au premier axe (même si leur corrélation au premier
# facteur reste supérieure à 0,6)

# Ce premier axe factoriel reflète classiquement un "effet taille" -> il exprime que certains individus ont de grandes
# valeurs pour l'ensemble des variables et d'autres de petites valeurs pour l'ensemble des variables.
# Cela indique que certaines communes ont des niveaux de délinquances/habitant (que ce soit en termes de I ou de V)
# globalement plus élevés quelque soit le type d'atteinte considéré.
# On va trouver le plus à droite du premier axe les communes qui ont un niveau de délinquance le plus élevé dans tous 
# les types d'infractions et au contraire à gauche les communes avec un niveau de délinquance le moins élevé.

# Par ailleurs, on constate que pour chaque type d'atteinte, les nombres de I et de V pour 1000 habitants sont très corrélés.
# Il ne semble pas y avoir de grande différences..., comme si lieu de domiciliation de la victime et lieu de l'infraction
# étaient peu dissociés au niveau communal... on a l'impression que la délinquance esr très concentrée spatialement dans 
# la dimension (I,V)
# Mais est-ce que ce résultat est vraiment robuste? ne pourrai-il pas être brouillé par le fait que l'on pourrait avoir une
# commune qui affiche des nombre de I et de V très proches, sans qu'il s'agisse des mêmes atteintes...
# et le problème est que l'on ne tient pas réellement compte des zonages d'étude.
# cf. stratégie 4 et 5 pour essayer d'y voir plus clair et améliorer l'analyse

# Interprétation du 2eme axe factoriel: 9,2% de l'inertie totale
# Ce second axe factoriel semble opposer les commune avec un niveau élevé d'atteintes physiques très violents (homicides et
# vols avec armes) aux communes avec un niveau faible de ces atteintes.
# Les variables "nombre d'homicides" et "nombre de vols avec armes" sont corrélées aux second axe avec un coeff de corrélation
# supérieur à 0.5
# WARNING: 
# Les communes 97358 et 97 208 sont très en haut à droite par rapport au reste du nuage.
# on zoome dessus:
zoom_outliers <- data_com_IV_acp2 %>% rownames_to_column(var="CODGEO") %>% filter(CODGEO %in% c("97358","97208"))
# Il s'agit de deux communes guyannaises qui semblent se distinguer par un niveau global de délinquance
# pour 1000 habitants très élevé, avec une "spécialisation" dans les homicides" et les "vols violents"

# question: faut-il se restreindre à la métropole dans l'étude?
# C) On peut refaire l'ACP en se restreignant à la seule métropole...

data_com_IV_acp2_metro <- data_com_IV_acp2 %>% filter(!DEP.x %in% c("971","972","973","974","976"))
summary(data_com_IV_acp2_metro)

res_acp_data_com_IV_acp3 <- PCA(data_com_IV_acp2_metro[3:28], scale.unit = TRUE, ncp = 5, graph = TRUE)
summary(res_acp_data_com_IV_acp3)

dimdesc(res_acp_data_com_IV_acp3)

# pour mettre en grisé les observations qui sont mal représentées sur le plan factoriel (Facteur 1, Facteur 2)
plot(res_acp_data_com_IV_acp3,select="cos2 0.7")

#res = PCAshiny(data_com_IV_acp2_metro)


# Stratégie 2:

# On fait toujours l'ACP en prenant les communes comme individus mais on modifie nos variables actives 
# on prend les nombres de I pour 1000 habitants par type d'atteinte (mesure de l'intensité de la délinquance)
# et on rajoute la structure des atteintes par type dans chaque commune (mesure la "spécialisation" de la commune
# dans telle ou telle atteinte)
# une variable de distance moyenne/médiane entre commune I et V pourrait être  pas mal pour mesurer
# la concentration spatiale de la délinquance


# Stratégie 3: 
# On fait l'ACP en prennant toujours les communes comme individus mais on se restreint ici aux seules atteintes physiques
# pour lesquelles le triplet de communes (I,V,M) est bien renseigné
# on considère les variables actives suivantes: le nombre de I, de V et de M pour 1000 habitants.



# Stratégie 4:
# On fait une ACP sur les communes
# et on prend comme variables actives: la part des atteintes associées à un couple (I,V) inscrit dans un même zonage (6 zonages possibles)
# et on peut rajouter une variable de spécialisation du type: la part de l'atteinte la plus présente dans
# la commune...

# Warning: on se restreint ici aux seules atteintes associées à un couple de communes (I,V) renseigné!

del2016_2021_IV_meme_zonage <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
  Nb_atteintes = sum(compteur, na.rm = TRUE),
  Nb_atteintes_cambr = sum(compteur*(classe2 == "Cambriolages de logement"), na.rm = TRUE),
  Nb_atteintes_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_atteintes_destr_degrad = sum(compteur*(classe2 == "Destructions et dégradations"), na.rm = TRUE),
  Nb_atteintes_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
  Nb_atteintes_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE),
  Nb_atteintes_vols_armes = sum(compteur*(classe2 == "Vols avec armes"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic = sum(compteur*(classe2 == "Vols dans les véhicules"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic = sum(compteur*(classe2 == "Vols de véhicules"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol = sum(compteur*(classe2 == "Vols sans violence contre des personnes"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme = sum(compteur*(classe2 == "Vols violents sans arme"), na.rm = TRUE),
  Nb_atteintes_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1ZE = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1ZE = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1ZE = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1ZE = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1ZE = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1ZE = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1ZE = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1ZE = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1ZE = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1ZE = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1BV = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1BV = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1BV = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1BV = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1BV = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1BV = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1BV = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1BV = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1BV = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1BV = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1GD = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1GD = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1GD = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1GD = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1GD = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1GD = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1GD = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1GD = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1GD = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1GD = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1UU = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1UU = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1UU = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1UU = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1UU = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1UU = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1UU = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1UU = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1UU = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1UU = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1AAV = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1AAV = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1AAV = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1AAV = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1AAV = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1AAV = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1AAV = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1AAV = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1AAV = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1AAV = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1CENTR = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1CENTR = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1CENTR = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1CENTR = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1CENTR = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1CENTR = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1CENTR = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1CENTR = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1CENTR = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1CENTR = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]

# on apparie ce fichier avec les infos socio-éco et démo sur les communes:

del2016_2021_IV_meme_zonage <- 
  merge(x = del2016_2021_IV_meme_zonage,
        y = infos_communes_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

del2016_2021_IV_meme_zonage <- 
  merge(x = del2016_2021_IV_meme_zonage,
        y = communes_zonages_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

# on transforme le tableau en tibble:
del2016_2021_IV_meme_zonage <- as_tibble(del2016_2021_IV_meme_zonage)

head(del2016_2021_IV_meme_zonage)
names(del2016_2021_IV_meme_zonage)

# On crée deux catégories d'atteintes (physiques/biens):
del2016_2021_IV_meme_zonage <- del2016_2021_IV_meme_zonage %>% 
  mutate(Nb_atteintes_biens = Nb_atteintes_cambr + Nb_atteintes_destr_degrad + 
           Nb_atteintes_vols_acces_vehic + Nb_atteintes_vols_ds_vehic + Nb_atteintes_vols_de_vehic + 
           Nb_atteintes_vols_sansviol,
         Nb_atteintes_physiques = Nb_atteintes_blessures_famil + Nb_atteintes_blessures_horsfamil + 
           Nb_atteintes_homic + Nb_atteintes_viol_sex + Nb_atteintes_vols_armes +
           Nb_atteintes_vols_violants_sansarme,
         Nb_atteintes_biens_1ZE = Nb_atteintes_cambr_1ZE + Nb_atteintes_destr_degrad_1ZE + 
           Nb_atteintes_vols_acces_vehic_1ZE + Nb_atteintes_vols_ds_vehic_1ZE + Nb_atteintes_vols_de_vehic_1ZE + 
           Nb_atteintes_vols_sansviol_1ZE,
         Nb_atteintes_physiques_1ZE = Nb_atteintes_blessures_famil_1ZE + Nb_atteintes_blessures_horsfamil_1ZE + 
           Nb_atteintes_homic_1ZE + Nb_atteintes_viol_sex_1ZE + Nb_atteintes_vols_armes_1ZE +
           Nb_atteintes_vols_violants_sansarme_1ZE,
         Nb_atteintes_biens_1BV = Nb_atteintes_cambr_1BV + Nb_atteintes_destr_degrad_1BV + 
           Nb_atteintes_vols_acces_vehic_1BV + Nb_atteintes_vols_ds_vehic_1BV + Nb_atteintes_vols_de_vehic_1BV + 
           Nb_atteintes_vols_sansviol_1BV,
         Nb_atteintes_physiques_1BV = Nb_atteintes_blessures_famil_1BV + Nb_atteintes_blessures_horsfamil_1BV + 
           Nb_atteintes_homic_1BV + Nb_atteintes_viol_sex_1BV + Nb_atteintes_vols_armes_1BV +
           Nb_atteintes_vols_violants_sansarme_1BV,
         Nb_atteintes_biens_1GD = Nb_atteintes_cambr_1GD + Nb_atteintes_destr_degrad_1GD + 
           Nb_atteintes_vols_acces_vehic_1GD + Nb_atteintes_vols_ds_vehic_1GD + Nb_atteintes_vols_de_vehic_1GD + 
           Nb_atteintes_vols_sansviol_1GD,
         Nb_atteintes_physiques_1GD = Nb_atteintes_blessures_famil_1GD + Nb_atteintes_blessures_horsfamil_1GD + 
           Nb_atteintes_homic_1GD + Nb_atteintes_viol_sex_1GD + Nb_atteintes_vols_armes_1GD +
           Nb_atteintes_vols_violants_sansarme_1GD,
         Nb_atteintes_biens_1UU = Nb_atteintes_cambr_1UU + Nb_atteintes_destr_degrad_1UU + 
           Nb_atteintes_vols_acces_vehic_1UU + Nb_atteintes_vols_ds_vehic_1UU + Nb_atteintes_vols_de_vehic_1UU + 
           Nb_atteintes_vols_sansviol_1UU,
         Nb_atteintes_physiques_1UU = Nb_atteintes_blessures_famil_1UU + Nb_atteintes_blessures_horsfamil_1UU + 
           Nb_atteintes_homic_1UU + Nb_atteintes_viol_sex_1UU + Nb_atteintes_vols_armes_1UU +
           Nb_atteintes_vols_violants_sansarme_1UU,
         Nb_atteintes_biens_1AAV = Nb_atteintes_cambr_1AAV + Nb_atteintes_destr_degrad_1AAV + 
           Nb_atteintes_vols_acces_vehic_1AAV + Nb_atteintes_vols_ds_vehic_1AAV + Nb_atteintes_vols_de_vehic_1AAV + 
           Nb_atteintes_vols_sansviol_1AAV,
         Nb_atteintes_physiques_1AAV = Nb_atteintes_blessures_famil_1AAV + Nb_atteintes_blessures_horsfamil_1AAV + 
           Nb_atteintes_homic_1AAV + Nb_atteintes_viol_sex_1AAV + Nb_atteintes_vols_armes_1AAV +
           Nb_atteintes_vols_violants_sansarme_1AAV,
         Nb_atteintes_biens_1CENTR= Nb_atteintes_cambr_1CENTR + Nb_atteintes_destr_degrad_1CENTR + 
           Nb_atteintes_vols_acces_vehic_1CENTR + Nb_atteintes_vols_ds_vehic_1CENTR + Nb_atteintes_vols_de_vehic_1CENTR + 
           Nb_atteintes_vols_sansviol_1CENTR,
         Nb_atteintes_physiques_1CENTR = Nb_atteintes_blessures_famil_1CENTR + Nb_atteintes_blessures_horsfamil_1CENTR + 
           Nb_atteintes_homic_1CENTR + Nb_atteintes_viol_sex_1CENTR + Nb_atteintes_vols_armes_1CENTR +
           Nb_atteintes_vols_violants_sansarme_1CENTR)

# On calcule pour chaque commune et chaque type d'atteinte: la proportion (en %) d'atteintes dont le couple (I,V) de communes
# s'inscrit dans un même zonage d'étude:

# a) Proportion d'atteintes dont le couple (I,V) de communes s'inscrit dans une même zone d'emploi (ZE):
del2016_2021_IV_meme_zonage$P_a_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$P_a_cambr_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$P_a_blessures_famil_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$P_a_blessures_horsfamil_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$P_a_destr_degrad_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$P_a_homic_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$P_a_viol_sex_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$P_a_vols_armes_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$P_a_vols_acces_vehic_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_ds_vehic_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_de_vehic_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_sansviol_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$P__vols_violants_sansarme_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100
del2016_2021_IV_meme_zonage$P_a_biens_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_biens_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_biens*100
del2016_2021_IV_meme_zonage$P_a_physiques_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_physiques_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_physiques*100

# b) Proportion d'atteintes dont le triplet de lieux s'inscrit dans un même bassin de vie (BV):
del2016_2021_IV_meme_zonage$P_a_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$P_a_cambr_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$P_a_blessures_famil_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$P_a_blessures_horsfamil_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$P_a_destr_degrad_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$P_a_homic_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$P_a_viol_sex_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$P_a_vols_armes_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$P_a_vols_acces_vehic_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_ds_vehic_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_de_vehic_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_sansviol_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$P__vols_violants_sansarme_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100
del2016_2021_IV_meme_zonage$P_a_biens_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_biens_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_biens*100
del2016_2021_IV_meme_zonage$P_a_physiques_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_physiques_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_physiques*100

# c) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même grille de densité (GD):
del2016_2021_IV_meme_zonage$P_a_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$P_a_cambr_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$P_a_blessures_famil_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$P_a_blessures_horsfamil_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$P_a_destr_degrad_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$P_a_homic_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$P_a_viol_sex_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$P_a_vols_armes_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$P_a_vols_acces_vehic_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_ds_vehic_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_de_vehic_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_sansviol_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$P__vols_violants_sansarme_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100
del2016_2021_IV_meme_zonage$P_a_biens_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_biens_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_biens*100
del2016_2021_IV_meme_zonage$P_a_physiques_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_physiques_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_physiques*100

# d) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même unité urbaine (UU):
del2016_2021_IV_meme_zonage$P_a_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$P_a_cambr_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$P_a_blessures_famil_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$P_a_blessures_horsfamil_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$P_a_destr_degrad_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$P_a_homic_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$P_a_viol_sex_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$P_a_vols_armes_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$P_a_vols_acces_vehic_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_ds_vehic_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_de_vehic_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_sansviol_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$P__vols_violants_sansarme_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100
del2016_2021_IV_meme_zonage$P_a_biens_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_biens_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_biens*100
del2016_2021_IV_meme_zonage$P_a_physiques_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_physiques_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_physiques*100

# e) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même aire d'attraction des villes (AAV):
del2016_2021_IV_meme_zonage$P_a_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$P_a_cambr_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$P_a_blessures_famil_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$P_a_blessures_horsfamil_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$P_a_destr_degrad_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$P_a_homic_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$P_a_viol_sex_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$P_a_vols_armes_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$P_a_vols_acces_vehic_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_ds_vehic_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_de_vehic_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_sansviol_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$P__vols_violants_sansarme_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100
del2016_2021_IV_meme_zonage$P_a_biens_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_biens_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_biens*100
del2016_2021_IV_meme_zonage$P_a_physiques_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_physiques_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_physiques*100

# f) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même centralité (CENTR) au sens de l'INRAE:
del2016_2021_IV_meme_zonage$P_a_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$P_a_cambr_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$P_a_blessures_famil_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$P_a_blessures_horsfamil_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$P_a_destr_degrad_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$P_a_homic_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$P_a_viol_sex_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$P_a_vols_armes_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$P_a_vols_acces_vehic_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_ds_vehic_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_de_vehic_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$P_a_vols_sansviol_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$P__vols_violants_sansarme_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100
del2016_2021_IV_meme_zonage$P_a_biens_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_biens_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_biens*100
del2016_2021_IV_meme_zonage$P_a_physiques_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_physiques_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_physiques*100

# On ajoute enfin des variables mesurant au niveau communal le nombre d'atteintes pour 1000 habitants:
del2016_2021_IV_meme_zonage$P_a_biens_1000hbts <-del2016_2021_IV_meme_zonage$Nb_atteintes_biens/del2016_2021_IV_meme_zonage$P19_POP*1000
del2016_2021_IV_meme_zonage$P_a_physiques_1000hbts <-del2016_2021_IV_meme_zonage$Nb_atteintes_physiques/del2016_2021_IV_meme_zonage$P19_POP*1000
# Passage en log:
del2016_2021_IV_meme_zonage$l_P_a_biens_1000hbts <-log(del2016_2021_IV_meme_zonage$P_a_biens_1000hbts)
del2016_2021_IV_meme_zonage$l_P_a_physiques_1000hbts <-log(del2016_2021_IV_meme_zonage$P_a_physiques_1000hbts)

names(del2016_2021_IV_meme_zonage)

# Filtrage de la base pour l'ACP:
# individus actifs: communes de la France métropolitaine
# variables actives: 12 + 2
# - proportion d'atteintes (biens/physiques) dont les communes (I,V) s'inscrivent dans un même zonage z (z=ZE, BV, UU, AAV, GD ou CENTR), en % 
# - nombre d'atteintes (biens/physiques) pour 1000 habitants.

# WARNING: Comme UU et AAV ne forment pas une partition du territoire français, on ne peut les faire rentrer dans l'ACP
# que pour les cog_com_22_inf font partie d'une UU et d'une AAV (on teste alors si cog_com_22_inf est rattachée 
# à une UU (UU2020 différent de "01000") ou à une AAV (AAV2020 différent de "OOO"))
# Pas de problème en revanche pour les autres zonages, car ils forment une partition du territoire français.

# on veut tester tous les types de zonages dans l'ACP (y compris les UU et les AAV):

#liste des restrictions sur les lignes:
# 1) on exclue les communes hors métropole (profil particulier de la délinquance);
# 2) on exclue les communes hors UU et hors AAV
# 3) on exclue les communes pour lesquelles on ne comptabilise aucune atteinte sur les biens et
# aucune atteintes sur les personnes
# 4) on ne garde que les communes qui ont au moins 500 habitants en 2019 (sinon R n'arrive pas à tourner sur l'ensemble
# des communes!)

data_com_IV_acp4 <- del2016_2021_IV_meme_zonage %>% filter(!(DEP.x %in% c("971","972","973","974","976")) &
                                                             !(UU2020=="01000") & !(AAV2020=="000") &
                                                           Nb_atteintes_biens>0 & Nb_atteintes_physiques>0 &
                                                           P19_POP>500) %>%
                    select(cog_com_22_inf,LIBGEO.x,P19_POP,l_P_a_biens_1000hbts,l_P_a_physiques_1000hbts,
                           contains(c("P_a_biens","P_a_physiques"))) %>%
                    column_to_rownames(var="cog_com_22_inf")
# il reste 24 836 communes (individus de l'ACP) 
summary(data_com_IV_acp4)

res_acp_data_com_IV_acp3 <- PCA(data_com_IV_acp4[3:14], scale.unit = TRUE, ncp = 5, graph = TRUE)
summary(res_acp_data_com_IV_acp3)

dimdesc(res_acp_data_com_IV_acp3)

# pour mettre en grisé les observations qui sont mal représentées sur le plan factoriel (Facteur 1, Facteur 2)
plot(res_acp_data_com_IV_acp3,select="cos2 0.7")

# data_com_IV_acp4_shiny <- data_com_IV_acp4[,3:16]
# res = PCAshiny(data_com_IV_acp4_shiny)







