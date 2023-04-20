
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

res = PCAshiny(data_com_IV_acp2[3:28])
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

plot(res_acp_data_com_IV_acp3,select="cos2 0.7")

res = PCAshiny(data_com_IV_acp3)


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









