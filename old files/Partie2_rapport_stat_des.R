
# Partie 2 du rapport Statapp: les fais stylisés (statistiques descriptives)

library(readr)
library(corrplot)
library(scatterplot3d)

# On commence par récupérer les bases construites dans le script R "Construction_base_etude.R":
# Lancer le programme:
#source("Construction_base_etude_GS3.R")

# Objectif de cette partie: 
# 1) étudier la distribution de l'indicateur "nombre d'infractions I pour 1000 habitants" calculé au niveau de la commune.
# 2) visualiser le nuage des communes dans les deux dimensions suivantes: nombre d'infractions I (en log) et
# taille de la commune (population en log) 
# 3) étudier la corrélation entre "nombre d'infractions I pour 1000 habitants" et "nombre de victimes V pour 1000 habitants"
# calculés au niveau communal (en se restreignant aux atteintes avec les communes de I et de V renseignées)
# 4) étudier la corrélation entre "nombre d'infractions I pour 1000 habitants", "nombre de victimes V pour 1000 habitants"
# et "nombre de mis en cause M pour 1000 habitants" calculés au niveau communal (en se restreignant ici aux atteintes induisant
# des dommages corporels, étant donné que ce sont celles où les communes de I, de V et de M sont les mieux renseignées)
# 5) tableaux sur les proportions d'atteintes par type selon que les communes (I,V) se trouvent dans un même zonage d'étude
# ou non / selon que les communes (I,V,M) se trouvent dans un même zonage d'étude ou non.

# Optionnel: zoom sur l'impact du Covid: refaire l'analyse des points 3) à 5) en séparant les périodes 2016-2019 et 
# 2020-2021.

# 1) étudier la distribution de l'indicateur "nombre d'infractions I pour 1000 habitants" calculé au niveau de la commune.

# calcul du nombre d'infractions I pour 1000 habitants au niveau de la commune:
# on se restreint aux atteintes pour lesquelles la commune de I est renseignée (contrainte très peu forte!)

del2016_2021_dt_com_inf <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE), .(
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
head(del2016_2021_dt_com_inf)

# On récupère l'info sur la population communale:
del2016_2021_dt_com_inf <- 
  merge(x = del2016_2021_dt_com_inf,
        y = infos_communes_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

del2016_2021_com_inf <- as_tibble(del2016_2021_dt_com_inf)

# On calcule les ratios "nombre d'infractions pour 1000 habitants" au niveau communal:
del2016_2021_dt_com_inf_pop <- del2016_2021_dt_com_inf %>% filter(P19_POP>1 & Nb_I>0)
del2016_2021_dt_com_inf_pop$log_Nb_I <-log(del2016_2021_dt_com_inf_pop$Nb_I)
del2016_2021_dt_com_inf_pop$Nb_I_pop <-del2016_2021_dt_com_inf_pop$Nb_I/del2016_2021_dt_com_inf_pop$P19_POP*1000
del2016_2021_dt_com_inf_pop$log_Nb_I_pop <-log(del2016_2021_dt_com_inf_pop$Nb_I_pop)

# Distribution du nombre d'infractions pour 1000 habitants:
summary(del2016_2021_dt_com_inf_pop)

# Représentation graphique de la distribution
hist(del2016_2021_dt_com_inf_pop$log_Nb_I_pop,
     main="",
     xlab = "Nombre d'infractions pour 1000 habitants (en log)",
     ylab = "Fréquence",
     col = "black",
     border = "white")


# on peut rajouter la densité d'une loi normale de moyenne hat_mu et hat_sigma:
hat_mu=mean(del2016_2021_dt_com_inf_pop$log_Nb_I_pop)
hat_sigma=sd(del2016_2021_dt_com_inf_pop$log_Nb_I_pop)
print(hat_mu)
## 4.268244
print(hat_sigma)
## 0.9500801
xtheo=1:8
ytheo=dnorm(xtheo, hat_mu,hat_sigma)

# hist(del2016_2021_dt_com_inf_pop$log_Nb_I_pop,
#      main = "Distribution du nombre d'infractions pour 1000 habitants au niveau communal",
#      xlab = "Nombre d'infractions pour 1000 habitants (en log)",
#      ylab = "Fréquence",
#      col = "black",
#      border = "white",
#      prob=T)
# points(xtheo,ytheo, col="lightslateblue",type="l", lwd=2)

# test de normalité (QQ-plot)
x <- del2016_2021_dt_com_inf_pop$log_Nb_I_pop
quantiles_theoriques=qnorm((1:length(x))/length(x),hat_mu,hat_sigma)
qqplot(quantiles_theoriques,x)
abline(a=0,b=1, col="lightslateblue")

# 2) visualiser le nuage des communes dans les deux dimensions suivantes: nombre d'infractions I (en log) et
# taille de la commune (population en log) 

del2016_2021_dt_com_inf_pop$log_P19_POP <-log(del2016_2021_dt_com_inf_pop$P19_POP)

plot(del2016_2021_dt_com_inf_pop$log_P19_POP,
     del2016_2021_dt_com_inf_pop$log_Nb_I,
     xlab="Taille de la commune (log de la population)",
     ylab = "Nombre d'infractions (en log)")
# forme caractéristique en cône oblique...

# 3) étudier la corrélation entre "nombre d'infractions I pour 1000 habitants" et "nombre de victimes V pour 1000 habitants"
# calculés au niveau communal (en se restreignant aux atteintes avec les communes de I et de V renseignées)

# calcul du nombre d'infractions I et du nombre de victimes V pour 1000 habitants au niveau de la commune:
# on se restreint aux atteintes pour lesquelles les commune de I et de V sont renseignées (on perd environ 60 000 atteintes
# sur 6,5 millions)

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

# On calcule les ratios "nombre d'infractions pour 1000 habitants" au niveau communal:
del2016_2021_com_IV <- del2016_2021_com_IV %>% filter(P19_POP>1 & Nb_I>0 & Nb_V>0)
del2016_2021_com_IV$log_Nb_I <-log(del2016_2021_com_IV$Nb_I)
del2016_2021_com_IV$Nb_I_pop <-del2016_2021_com_IV$Nb_I/del2016_2021_com_IV$P19_POP*1000
del2016_2021_com_IV$log_Nb_I_pop <-log(del2016_2021_com_IV$Nb_I_pop)
del2016_2021_com_IV$log_Nb_V <-log(del2016_2021_com_IV$Nb_V)
del2016_2021_com_IV$Nb_V_pop <-del2016_2021_com_IV$Nb_V/del2016_2021_com_IV$P19_POP*1000
del2016_2021_com_IV$log_Nb_V_pop <-log(del2016_2021_com_IV$Nb_V_pop)

summary(del2016_2021_com_IV)

# Visualisation du lien entre nombre d'infractions pour 1000 habitants et nombre de victimes pour 1000 habitants
# on peut apprécier la distance du nuage à la première bissectrice (situation où le nombre d'infractions et de victimes
# sont égaux dans une commune, ce qui traduit le fait que les victimes habitent la commune où elles ont été victimes de 
# l'atteinte"):
plot(del2016_2021_com_IV$log_Nb_I_pop,
     del2016_2021_com_IV$log_Nb_V_pop,
     xlab="Nombre d'infractions pour 1000 habitants (en log)",
     ylab = "Nombre de victimes pour 1000 habitants (en log)")
abline(a=0,b=1,col="red")

# Matrice de corrélation: on croise les deux indicateurs (I, V) pour chaque type d'atteinte
# on doit calculer les ratios pour les différents types d'atteinte
#TODO
# Ou autre option: on peut répliquer le même scatter plot mais sur deux sous-groupes d'atteintes: biens vs physiques.
# et comparer si les deux nuages sont à la même distance de la première bissectrice... TODO!

del2016_2021_com_IV_corr <- del2016_2021_com_IV %>% select(log_Nb_I_pop,log_Nb_V_pop)

mcor1 <- cor(del2016_2021_com_IV_corr)
corrplot(mcor1, type="upper", order="hclust", tl.col="black", tl.srt=45)
# on trouve un coeff de corrélation de 0,9...
reg<-lm(log_Nb_I_pop ~ log_Nb_V_pop, data = del2016_2021_com_IV_corr)
summary(reg)
coefficients(reg)
# mais comme ce n'est pas gaussien, difficile d'interpréter les t-tests!
# Et puis il faut voir si on retrouve 

# 4) étudier la corrélation entre "nombre d'infractions I pour 1000 habitants", "nombre de victimes V pour 1000 habitants"
# et "nombre de mis en cause M pour 1000 habitants" calculés au niveau communal (en se restreignant ici aux atteintes induisant
# des dommages corporels, étant donné que ce sont celles où les communes de I, de V et de M sont les mieux renseignées)

# calcul du nombre d'infractions I, du nombre de victimes V et du nombre de mis en cause pour 1000 habitants au niveau de
# la commune: on se restreint aux atteintes pour lesquelles les commune de I, de V et de M sont renseignées 
# d'après le tableau 1 de la partie sur les données, l'info sur le triplet de commune (I,V,M) n'est bien renseigné que pour 
# les atteintes physiques, et en particulier pour les coups et blessures volontaires. On se focalisera sur ce type 
# d'atteintes corporelles.

del2016_2021_dt_com_I2 <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE)
                                           & (classe2 %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                                               "Coups et blessures volontaires en dehors de la sphère familiale",
                                                               "Homicides","Violences sexuelles")), .(
  Nb_I_physiques = sum(compteur, na.rm = TRUE),
  Nb_I_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_I_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_I_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
  Nb_I_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]

del2016_2021_dt_com_V2 <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE)
                                            & (classe2 %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                                                "Coups et blessures volontaires en dehors de la sphère familiale",
                                                                "Homicides","Violences sexuelles")), .(
                                                                  Nb_V_physiques = sum(compteur, na.rm = TRUE),
                                                                  Nb_V_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
                                                                  Nb_V_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
                                                                  Nb_V_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
                                                                  Nb_V_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE)),
                                            by = .(cog_com_22_vict)]


del2016_2021_dt_com_M2 <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE)
                                            & (classe2 %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                                                "Coups et blessures volontaires en dehors de la sphère familiale",
                                                                "Homicides","Violences sexuelles")), .(
                                                                  Nb_M_physiques = sum(compteur, na.rm = TRUE),
                                                                  Nb_M_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
                                                                  Nb_M_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
                                                                  Nb_M_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
                                                                  Nb_M_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE)),
                                            by = .(cog_com_22_mec)]

head(del2016_2021_dt_com_M2)


# on apparie ces 3 fichiers avec le fichier "communes_zonages" reliant chaque commune aux différents zonages
# administratifs et d'étude de l'Insee:

# warning: on fait une jointure à gauche de communes_zonages_dt, de sorte à avoir l'ensemble des communes
# répertoriées par l'Insee récemment (population de référence):

del2016_2021_dt_com_I2 <- 
  merge(x = communes_zonages_dt,
        y = del2016_2021_dt_com_I2,
        by.x = "CODGEO",
        by.y = "cog_com_22_inf",
        all.x = TRUE)
del2016_2021_dt_com_IV2 <- 
  merge(x = del2016_2021_dt_com_I2,
        y = del2016_2021_dt_com_V2,
        by.x = "CODGEO",
        by.y = "cog_com_22_vict",
        all.x = TRUE)
del2016_2021_dt_com_IVM <- 
  merge(x = del2016_2021_dt_com_IV2,
        y = del2016_2021_dt_com_M2,
        by.x = "CODGEO",
        by.y = "cog_com_22_mec",
        all.x = TRUE)
rm(del2016_2021_dt_com_I2)
rm(del2016_2021_dt_com_V2)
rm(del2016_2021_dt_com_IV2)

# on apparie ce fichier avec les infos socio-éco et démo sur les communes:

del2016_2021_dt_com_IVM <- 
  merge(x = del2016_2021_dt_com_IVM,
        y = infos_communes_dt,
        by.x = "CODGEO",
        by.y = "CODGEO",
        all.x = TRUE)

names(del2016_2021_dt_com_IVM)

del2016_2021_com_IVM <- as_tibble(del2016_2021_dt_com_IVM)

# On calcule les ratios "nombre d'atteintes physiques pour 1000 habitants" au niveau communal:
del2016_2021_com_IVM <- del2016_2021_com_IVM %>% filter(P19_POP>1 & Nb_I_physiques>0 & Nb_V_physiques>0 & Nb_M_physiques>0)

del2016_2021_com_IVM$log_Nb_I_physiques <-log(del2016_2021_com_IVM$Nb_I_physiques)
del2016_2021_com_IVM$Nb_I_physiques_pop <-del2016_2021_com_IVM$Nb_I_physiques/del2016_2021_com_IVM$P19_POP*1000
del2016_2021_com_IVM$log_Nb_I_physiques_pop <-log(del2016_2021_com_IVM$Nb_I_physiques_pop)

del2016_2021_com_IVM$log_Nb_V_physiques <-log(del2016_2021_com_IVM$Nb_V_physiques)
del2016_2021_com_IVM$Nb_V_physiques_pop <-del2016_2021_com_IVM$Nb_V_physiques/del2016_2021_com_IVM$P19_POP*1000
del2016_2021_com_IVM$log_Nb_V_physiques_pop <-log(del2016_2021_com_IVM$Nb_V_physiques_pop)

del2016_2021_com_IVM$log_Nb_M_physiques <-log(del2016_2021_com_IVM$Nb_M_physiques)
del2016_2021_com_IVM$Nb_M_physiques_pop <-del2016_2021_com_IVM$Nb_M_physiques/del2016_2021_com_IVM$P19_POP*1000
del2016_2021_com_IVM$log_Nb_M_physiques_pop <-log(del2016_2021_com_IVM$Nb_M_physiques_pop)

summary(del2016_2021_com_IVM)

# Visualisation du lien entre nombre d'atteintes physiques pour 1000 habitants, nombre de victimes pour 1000 habitants et
# nombre de mis en cause pour 1000 habutants à l'échelle communale:

scatterplot3d(del2016_2021_com_IVM[,c("log_Nb_I_physiques_pop","log_Nb_V_physiques_pop","log_Nb_M_physiques_pop")],
              main = "",
              xlab = "Nombre d'infractions pour 1000 hab. (en log)",
              ylab = "Nombre de victimes pour 1000 hab. (en log)",
              zlab = "Nombre de mis en cause pour 1000 hab (en log)")

head(del2016_2021_com_IVM)
names(del2016_2021_com_IVM)
del2016_2021_com_IVM[,c("CODGEO","log_Nb_I_physiques_pop")]
                        
# 5) tableaux sur les proportions d'atteintes par type selon que les communes (I,V) se trouvent dans un même zonage d'étude
# ou non / selon que les communes (I,V,M) se trouvent dans un même zonage d'étude ou non.

#a) Tableau qui décrit pour chaque type d'atteintes, la proportions d'atteintes associées à un couple de communes (I,V) 
# se situant dans un même zonage d'étude:

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
  Nb_atteintes_vols_violants_sansarme_1CENTR = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE))]

head(del2016_2021_IV_meme_zonage)

# On calcule pour chaque commune et chaque type d'atteinte: la proportion (en %) d'atteintes dont le couple (I,V) de communes
# s'inscrit dans un même zonage d'étude:

# a) Proportion d'atteintes dont le couple (I,V) de communes s'inscrit dans une même zone d'emploi (ZE):
del2016_2021_IV_meme_zonage$Prop_atteintes_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_cambr_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_famil_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_horsfamil_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_destr_degrad_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$Prop_atteintes_homic_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_viol_sex_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_armes_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_acces_vehic_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_ds_vehic_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_de_vehic_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_sansviol_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_violants_sansarme_1ZE <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1ZE/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100

# b) Proportion d'atteintes dont le triplet de lieux s'inscrit dans un même bassin de vie (BV):
del2016_2021_IV_meme_zonage$Prop_atteintes_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_cambr_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_famil_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_horsfamil_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_destr_degrad_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$Prop_atteintes_homic_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_viol_sex_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_armes_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_acces_vehic_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_ds_vehic_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_de_vehic_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_sansviol_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_violants_sansarme_1BV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1BV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100

# c) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même grille de densité (GD):
del2016_2021_IV_meme_zonage$Prop_atteintes_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_cambr_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_famil_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_horsfamil_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_destr_degrad_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$Prop_atteintes_homic_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_viol_sex_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_armes_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_acces_vehic_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_ds_vehic_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_de_vehic_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_sansviol_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_violants_sansarme_1GD <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1GD/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100

# d) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même unité urbaine (UU):
del2016_2021_IV_meme_zonage$Prop_atteintes_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_cambr_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_famil_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_horsfamil_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_destr_degrad_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$Prop_atteintes_homic_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_viol_sex_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_armes_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_acces_vehic_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_ds_vehic_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_de_vehic_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_sansviol_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_violants_sansarme_1UU <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1UU/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100

# e) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même aire d'attraction des villes (AAV):
del2016_2021_IV_meme_zonage$Prop_atteintes_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_cambr_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_famil_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_horsfamil_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_destr_degrad_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$Prop_atteintes_homic_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_viol_sex_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_armes_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_acces_vehic_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_ds_vehic_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_de_vehic_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_sansviol_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_violants_sansarme_1AAV <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1AAV/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100

# f) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même centralité (CENTR) au sens de l'INRAE:
del2016_2021_IV_meme_zonage$Prop_atteintes_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_cambr_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_cambr_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_cambr*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_famil_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_blessures_horsfamil_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IV_meme_zonage$Prop_atteintes_destr_degrad_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_destr_degrad*100
del2016_2021_IV_meme_zonage$Prop_atteintes_homic_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_homic_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_viol_sex_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_viol_sex*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_armes_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_armes*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_acces_vehic_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_acces_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_ds_vehic_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_ds_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_de_vehic_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_de_vehic*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_sansviol_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_sansviol*100
del2016_2021_IV_meme_zonage$Prop_atteintes_vols_violants_sansarme_1CENTR <-del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme_1CENTR/del2016_2021_IV_meme_zonage$Nb_atteintes_vols_violants_sansarme*100

head(del2016_2021_IV_meme_zonage)

#on transforme le tableau en tibble:
t_del2016_2021_IV_meme_zonage <- as_tibble(del2016_2021_IV_meme_zonage)

# On crée le tableau final pour le rapport:
# 13 lignes -> 12 types d'atteinte + ligne "Ensemble des atteintes"
# 6 colonnes -> 6 zonages d'étude (2 zonages morphologiques: UU et GD + 4 zonages fonctionnels: ZE ,BV, AAV, centralités)

classe <- c("Ensemble","Vols sans violence contre des personnes","Destructions et dégradations","Vols dans les véhicules",
            "Cambriolages de logement","Vols de véhicules","Coups et blessures volontaires en dehors de la sphère familiale",
            "Coups et blessures volontaires dans la sphère familiale","Vols d'accessoires sur véhicules",
            "Vols violents sans arme","Violences sexuelles","Vols avec armes","Homicides")

type_atteinte <- data.frame(classe)
type_atteinte <- as_tibble(type_atteinte)

# Mise en forme du tableau pour le rapport:

# UU:
t_del2016_2021_IV_meme_UU <- t_del2016_2021_IV_meme_zonage %>% select(Prop_atteintes_1UU,Prop_atteintes_vols_sansviol_1UU,
                                                                    Prop_atteintes_destr_degrad_1UU,Prop_atteintes_vols_ds_vehic_1UU,
                                                                    Prop_atteintes_cambr_1UU,Prop_atteintes_vols_de_vehic_1UU,
                                                                    Prop_atteintes_blessures_horsfamil_1UU,Prop_atteintes_blessures_famil_1UU,
                                                                    Prop_atteintes_vols_acces_vehic_1UU,Prop_atteintes_vols_violants_sansarme_1UU,
                                                                    Prop_atteintes_viol_sex_1UU,Prop_atteintes_vols_armes_1UU,Prop_atteintes_homic_1UU) %>%
                                                          pivot_longer(cols = starts_with("Prop"), 
                                                          names_to = "type_atteinte", 
                                                          values_to = "Prop_atteintes_IV_meme_UU") %>% select(-type_atteinte) %>% 
                                                          mutate(Prop_atteintes_IV_meme_UU = round(Prop_atteintes_IV_meme_UU,1))
# GD:
t_del2016_2021_IV_meme_GD <- t_del2016_2021_IV_meme_zonage %>% select(Prop_atteintes_1GD,Prop_atteintes_vols_sansviol_1GD,
                                                                      Prop_atteintes_destr_degrad_1GD,Prop_atteintes_vols_ds_vehic_1GD,
                                                                      Prop_atteintes_cambr_1GD,Prop_atteintes_vols_de_vehic_1GD,
                                                                      Prop_atteintes_blessures_horsfamil_1GD,Prop_atteintes_blessures_famil_1GD,
                                                                      Prop_atteintes_vols_acces_vehic_1GD,Prop_atteintes_vols_violants_sansarme_1GD,
                                                                      Prop_atteintes_viol_sex_1GD,Prop_atteintes_vols_armes_1GD,Prop_atteintes_homic_1GD) %>%
                                                           pivot_longer(cols = starts_with("Prop"), 
                                                           names_to = "type_atteinte", 
                                                           values_to = "Prop_atteintes_IV_meme_GD") %>% select(-type_atteinte) %>%
                                                           mutate(Prop_atteintes_IV_meme_GD = round(Prop_atteintes_IV_meme_GD,1))
# ZE:
t_del2016_2021_IV_meme_ZE <- t_del2016_2021_IV_meme_zonage %>% select(Prop_atteintes_1ZE,Prop_atteintes_vols_sansviol_1ZE,
                                                                      Prop_atteintes_destr_degrad_1ZE,Prop_atteintes_vols_ds_vehic_1ZE,
                                                                      Prop_atteintes_cambr_1ZE,Prop_atteintes_vols_de_vehic_1ZE,
                                                                      Prop_atteintes_blessures_horsfamil_1ZE,Prop_atteintes_blessures_famil_1ZE,
                                                                      Prop_atteintes_vols_acces_vehic_1ZE,Prop_atteintes_vols_violants_sansarme_1ZE,
                                                                      Prop_atteintes_viol_sex_1ZE,Prop_atteintes_vols_armes_1ZE,Prop_atteintes_homic_1ZE) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IV_meme_ZE") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IV_meme_ZE = round(Prop_atteintes_IV_meme_ZE,1))
# BV:
t_del2016_2021_IV_meme_BV <- t_del2016_2021_IV_meme_zonage %>% select(Prop_atteintes_1BV,Prop_atteintes_vols_sansviol_1BV,
                                                                      Prop_atteintes_destr_degrad_1BV,Prop_atteintes_vols_ds_vehic_1BV,
                                                                      Prop_atteintes_cambr_1BV,Prop_atteintes_vols_de_vehic_1BV,
                                                                      Prop_atteintes_blessures_horsfamil_1BV,Prop_atteintes_blessures_famil_1BV,
                                                                      Prop_atteintes_vols_acces_vehic_1BV,Prop_atteintes_vols_violants_sansarme_1BV,
                                                                      Prop_atteintes_viol_sex_1BV,Prop_atteintes_vols_armes_1BV,Prop_atteintes_homic_1BV) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IV_meme_BV") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IV_meme_BV = round(Prop_atteintes_IV_meme_BV,1))
# AAV:
t_del2016_2021_IV_meme_AAV <- t_del2016_2021_IV_meme_zonage %>% select(Prop_atteintes_1AAV,Prop_atteintes_vols_sansviol_1AAV,
                                                                      Prop_atteintes_destr_degrad_1AAV,Prop_atteintes_vols_ds_vehic_1AAV,
                                                                      Prop_atteintes_cambr_1AAV,Prop_atteintes_vols_de_vehic_1AAV,
                                                                      Prop_atteintes_blessures_horsfamil_1AAV,Prop_atteintes_blessures_famil_1AAV,
                                                                      Prop_atteintes_vols_acces_vehic_1AAV,Prop_atteintes_vols_violants_sansarme_1AAV,
                                                                      Prop_atteintes_viol_sex_1AAV,Prop_atteintes_vols_armes_1AAV,Prop_atteintes_homic_1AAV) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IV_meme_AAV") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IV_meme_AAV = round(Prop_atteintes_IV_meme_AAV,1))
# Centralités (INRAE):
t_del2016_2021_IV_meme_CENTR <- t_del2016_2021_IV_meme_zonage %>% select(Prop_atteintes_1CENTR,Prop_atteintes_vols_sansviol_1CENTR,
                                                                       Prop_atteintes_destr_degrad_1CENTR,Prop_atteintes_vols_ds_vehic_1CENTR,
                                                                       Prop_atteintes_cambr_1CENTR,Prop_atteintes_vols_de_vehic_1CENTR,
                                                                       Prop_atteintes_blessures_horsfamil_1CENTR,Prop_atteintes_blessures_famil_1CENTR,
                                                                       Prop_atteintes_vols_acces_vehic_1CENTR,Prop_atteintes_vols_violants_sansarme_1CENTR,
                                                                       Prop_atteintes_viol_sex_1CENTR,Prop_atteintes_vols_armes_1CENTR,Prop_atteintes_homic_1CENTR) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IV_meme_CENTR") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IV_meme_CENTR = round(Prop_atteintes_IV_meme_CENTR,1))

# Tableau à intégrer dans le Rapport:

tableau4 <-cbind(type_atteinte,t_del2016_2021_IV_meme_UU,t_del2016_2021_IV_meme_GD,t_del2016_2021_IV_meme_ZE,
             t_del2016_2021_IV_meme_BV,t_del2016_2021_IV_meme_AAV,t_del2016_2021_IV_meme_CENTR)

# Export CSV:
write.csv(tableau4, "/Users/sklenard/Documents/Statapp/Figures-Rapport Statapp//Tableau4.csv", row.names=FALSE)


#b) Tableau qui décrit pour chaque type d'atteintes, la proportions d'atteintes associées à un triplet de communes (I,V,M) 
# se situant dans un même zonage d'étude (on se limitera ici aux atteintes physiques)

# Warning: on se restreint ici aux seules atteintes physiques
# associées à un couple de communes (I,V,M) renseigné!

del2016_2021_IVM_meme_zonage <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                                                   (is.na(cog_com_22_mec)==FALSE) & 
                                                   (classe2 %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                                                     "Coups et blessures volontaires en dehors de la sphère familiale",
                                                                     "Homicides","Violences sexuelles")), .(
  Nb_atteintes = sum(compteur, na.rm = TRUE),
  Nb_atteintes_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_atteintes_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
  Nb_atteintes_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE),
  Nb_atteintes_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1ZE = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1ZE = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1BV = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1BV = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1GD = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1GD = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1UU = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1UU = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1AAV = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1AAV = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1CENTR = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1CENTR = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE))]

head(del2016_2021_IVM_meme_zonage)

# On calcule pour chaque commune et chaque type d'atteinte: la proportion (en %) d'atteintes dont le triplet (I,V,M) de communes
# s'inscrit dans un même zonage d'étude:

# a) Proportion d'atteintes dont le triplet (I,V,M) de communes s'inscrit dans une même zone d'emploi (ZE):
del2016_2021_IVM_meme_zonage$Prop_atteintes_1ZE <-del2016_2021_IVM_meme_zonage$Nb_atteintes_1ZE/del2016_2021_IVM_meme_zonage$Nb_atteintes*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_famil_1ZE <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil_1ZE/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_horsfamil_1ZE <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil_1ZE/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_homic_1ZE <-del2016_2021_IVM_meme_zonage$Nb_atteintes_homic_1ZE/del2016_2021_IVM_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_viol_sex_1ZE <-del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex_1ZE/del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex*100

# b) Proportion d'atteintes dont le triplet de lieux s'inscrit dans un même bassin de vie (BV):
del2016_2021_IVM_meme_zonage$Prop_atteintes_1BV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_1BV/del2016_2021_IVM_meme_zonage$Nb_atteintes*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_famil_1BV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil_1BV/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_horsfamil_1BV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil_1BV/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_homic_1BV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_homic_1BV/del2016_2021_IVM_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_viol_sex_1BV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex_1BV/del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex*100

# c) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même grille de densité (GD):
del2016_2021_IVM_meme_zonage$Prop_atteintes_1GD <-del2016_2021_IVM_meme_zonage$Nb_atteintes_1GD/del2016_2021_IVM_meme_zonage$Nb_atteintes*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_famil_1GD <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil_1GD/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_horsfamil_1GD <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil_1GD/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_homic_1GD <-del2016_2021_IVM_meme_zonage$Nb_atteintes_homic_1GD/del2016_2021_IVM_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_viol_sex_1GD <-del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex_1GD/del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex*100

# d) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même unité urbaine (UU):
del2016_2021_IVM_meme_zonage$Prop_atteintes_1UU <-del2016_2021_IVM_meme_zonage$Nb_atteintes_1UU/del2016_2021_IVM_meme_zonage$Nb_atteintes*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_famil_1UU <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil_1UU/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_horsfamil_1UU <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil_1UU/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_homic_1UU <-del2016_2021_IVM_meme_zonage$Nb_atteintes_homic_1UU/del2016_2021_IVM_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_viol_sex_1UU <-del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex_1UU/del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex*100

# e) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même aire d'attraction des villes (AAV):
del2016_2021_IVM_meme_zonage$Prop_atteintes_1AAV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_1AAV/del2016_2021_IVM_meme_zonage$Nb_atteintes*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_famil_1AAV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil_1AAV/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_horsfamil_1AAV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil_1AAV/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_homic_1AAV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_homic_1AAV/del2016_2021_IVM_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_viol_sex_1AAV <-del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex_1AAV/del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex*100

# f) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même centralité (CENTR) au sens de l'INRAE:
del2016_2021_IVM_meme_zonage$Prop_atteintes_1CENTR <-del2016_2021_IVM_meme_zonage$Nb_atteintes_1CENTR/del2016_2021_IVM_meme_zonage$Nb_atteintes*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_famil_1CENTR <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil_1CENTR/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_famil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_blessures_horsfamil_1CENTR <-del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil_1CENTR/del2016_2021_IVM_meme_zonage$Nb_atteintes_blessures_horsfamil*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_homic_1CENTR <-del2016_2021_IVM_meme_zonage$Nb_atteintes_homic_1CENTR/del2016_2021_IVM_meme_zonage$Nb_atteintes_homic*100
del2016_2021_IVM_meme_zonage$Prop_atteintes_viol_sex_1CENTR <-del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex_1CENTR/del2016_2021_IVM_meme_zonage$Nb_atteintes_viol_sex*100

head(del2016_2021_IVM_meme_zonage)

#on transforme le tableau en tibble:
t_del2016_2021_IVM_meme_zonage <- as_tibble(del2016_2021_IVM_meme_zonage)

# On crée le tableau final pour le rapport:
# 5 lignes -> 4 types d'atteinte physique + ligne "Ensemble des atteintes physiques"
# 6 colonnes -> 6 zonages d'étude (2 zonages morphologiques: UU et GD + 4 zonages fonctionnels: ZE ,BV, AAV, centralités)

classe_physique <- c("Ensemble","Coups et blessures volontaires en dehors de la sphère familiale",
            "Coups et blessures volontaires dans la sphère familiale",
            "Violences sexuelles","Homicides")

type_atteinte_physique <- data.frame(classe_physique)
type_atteinte_physique <- as_tibble(type_atteinte_physique)

# Mise en forme du tableau pour le rapport:

# UU:
t_del2016_2021_IVM_meme_UU <- t_del2016_2021_IVM_meme_zonage %>% select(Prop_atteintes_1UU,
                                                                      Prop_atteintes_blessures_horsfamil_1UU,Prop_atteintes_blessures_famil_1UU,
                                                                      Prop_atteintes_viol_sex_1UU,Prop_atteintes_homic_1UU) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IVM_meme_UU") %>% select(-type_atteinte) %>% 
  mutate(Prop_atteintes_IVM_meme_UU = round(Prop_atteintes_IVM_meme_UU,1))
# GD:
t_del2016_2021_IVM_meme_GD <- t_del2016_2021_IVM_meme_zonage %>% select(Prop_atteintes_1GD,
                                                                      Prop_atteintes_blessures_horsfamil_1GD,Prop_atteintes_blessures_famil_1GD,
                                                                      Prop_atteintes_viol_sex_1GD,Prop_atteintes_homic_1GD) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IVM_meme_GD") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IVM_meme_GD = round(Prop_atteintes_IVM_meme_GD,1))
# ZE:
t_del2016_2021_IVM_meme_ZE <- t_del2016_2021_IVM_meme_zonage %>% select(Prop_atteintes_1ZE,
                                                                      Prop_atteintes_blessures_horsfamil_1ZE,Prop_atteintes_blessures_famil_1ZE,
                                                                      Prop_atteintes_viol_sex_1ZE,Prop_atteintes_homic_1ZE) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IVM_meme_ZE") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IVM_meme_ZE = round(Prop_atteintes_IVM_meme_ZE,1))
# BV:
t_del2016_2021_IVM_meme_BV <- t_del2016_2021_IVM_meme_zonage %>% select(Prop_atteintes_1BV,
                                                                      Prop_atteintes_blessures_horsfamil_1BV,Prop_atteintes_blessures_famil_1BV,
                                                                      Prop_atteintes_viol_sex_1BV,Prop_atteintes_homic_1BV) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IVM_meme_BV") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IVM_meme_BV = round(Prop_atteintes_IVM_meme_BV,1))
# AAV:
t_del2016_2021_IVM_meme_AAV <- t_del2016_2021_IVM_meme_zonage %>% select(Prop_atteintes_1AAV,
                                                                       Prop_atteintes_blessures_horsfamil_1AAV,Prop_atteintes_blessures_famil_1AAV,
                                                                       Prop_atteintes_viol_sex_1AAV,Prop_atteintes_homic_1AAV) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IVM_meme_AAV") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IVM_meme_AAV = round(Prop_atteintes_IVM_meme_AAV,1))
# Centralités (INRAE):
t_del2016_2021_IVM_meme_CENTR <- t_del2016_2021_IVM_meme_zonage %>% select(Prop_atteintes_1CENTR,
                                                                         Prop_atteintes_blessures_horsfamil_1CENTR,Prop_atteintes_blessures_famil_1CENTR,
                                                                         Prop_atteintes_viol_sex_1CENTR,Prop_atteintes_homic_1CENTR) %>%
  pivot_longer(cols = starts_with("Prop"), 
               names_to = "type_atteinte", 
               values_to = "Prop_atteintes_IVM_meme_CENTR") %>% select(-type_atteinte) %>%
  mutate(Prop_atteintes_IVM_meme_CENTR = round(Prop_atteintes_IVM_meme_CENTR,1))

# Tableau à intégrer dans le Rapport:

tableau5 <-cbind(type_atteinte_physique,t_del2016_2021_IVM_meme_UU,t_del2016_2021_IVM_meme_GD,t_del2016_2021_IVM_meme_ZE,
                 t_del2016_2021_IVM_meme_BV,t_del2016_2021_IVM_meme_AAV,t_del2016_2021_IVM_meme_CENTR)

# Export CSV:
write.csv(tableau5, "/Users/sklenard/Documents/Statapp/Figures-Rapport Statapp//Tableau5.csv", row.names=FALSE)

# Optionnel: zoom sur l'impact du Covid: refaire l'analyse des points 3) à 5) en séparant les périodes 2016-2019 et 
# 2020-2021.

#TODO!

# On pourra mettre les tableaux en annexe du rapport, si des résultats importants s'en dégagent...


