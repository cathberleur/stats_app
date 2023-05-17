
# Partie 3 du mémoire: Classification des communes et des départements de France métropolitaine au regard de la délinquance


# A) ACP n°1: individus = 96 départements de la France métropolitaine.
# 16 variables actives: volume de la délinquance (8 types d'atteinte) + structure de la délinquance (8 types d'atteinte)
# 14 variables quantitatives illustratives: 7 (IV ds même commune/ zonage d'étude) + 7 (corpo_IVM ds commune/ même zonage d'étude)

# Tableau de données de l'ACP n°1:


data.ACP_1 <- delinquance_dep %>% filter (is.na(DEP_inf)==FALSE & !(DEP_inf %in% c("971","972","973","974","976"))) %>%
                              select(DEP_inf,
                                     # I. 16 variables actives:
                                     # a) 8 variables actives mesurant le volume de délinquance (pour 1000 hbts) pour chacune des 8 types 
                                     # d'atteinte:
                                     I_cambr,I_destr_degrad,I_vols_vehic,I_vols_sansviolence,I_vols_violents,I_viol_sex,I_bless_famil,
                                     I_bless_hfamil_homic,  
                                     # b) 8 variables actives mesurant la part (en %) de chaque type d'atteinte dans chaque département 
                                     # (structure de la délinquance par type d'atteinte):
                                     P_I_cambr,P_I_destr_degrad,P_I_vols_vehic,P_I_vols_sansviolence,P_I_vols_violents,P_I_viol_sex,
                                     P_I_bless_famil,P_I_bless_hfamil_homic,
                                     #II. variables qualitatives:
                                     #a) La part d'atteintes associées à un couple de communes (I,V) dans le même zonage:
                                     P_I_IV_1COM,P_I_IV_1ZE,P_I_IV_1BV,P_I_IV_1GD,P_I_IV_1UU,P_I_IV_1AAV,P_I_IV_1CENTR,
                                     # b) La part d'atteintes corporelles associées à un triplet de communes (I,V,M) dans le même zonage:
                                     P_I_corpo_IVM_1COM,P_I_corpo_IVM_1ZE,P_I_corpo_IVM_1BV,P_I_corpo_IVM_1GD,P_I_corpo_IVM_1UU,P_I_corpo_IVM_1AAV,P_I_corpo_IVM_1CENTR
                                     # # c) Indicateurs démographiques:
                                     # densite_pop,P_pop15_29ans, # calculer la part de communes classées dans la GD en "Urbain dense" (proxy d'un niveau d'urbanisation du département)
                                     # # d) Indicateurs socio-économiques:
                                     # part_chomeurs,MED20,R_P90_P10_MED20,part_pop_com_fragiles, 
                                     # # e) Indicateurs caractérisant les zones touristiques:
                                     # P_res_tourisme,P_camping_1000hbt,P_hotel,part_res_secondaires
                                     ) %>%
                              column_to_rownames(var="DEP_inf")

# On effectue l'ACP sur ce jeu de données:

res.PCA_1<-PCA(data.ACP_1,quanti.sup = 17:30,graph=FALSE)

# Sortie d'un rapport automatique:
# Investigate(res.PCA_1)

# Exploration/visualisation des résultats: à utiliser pour explorer les résultats/visualisations alternatives

#explor(res.PCA_1)

#res.PCA_1_shiny = PCAshiny(data.ACP_1)

# Valorisation des sorties de l'ACP dans le Mémoire
# (pour de jolies sorties, priviligiez le package factoextra)
# voir la page web très bien faite à ce sujet:
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/#valeurs-propres-variances

# 2) Les figures à retenir pour le corps principale du texte du mémoire:

# a) Barplot des % d'inertie expliquée par chaque axe factoriel:
scree.plot_ACP_1 <-fviz_eig(res.PCA_1, addlabels = TRUE, ylim = c(0, 50))
scree.plot_ACP_1

# b) Cercle de corrélation des variables (actives + supplémentaires):

# Axes factoriels 1 et 2:
plot_PCA_1_var_axes1_2 <- plot.PCA(res.PCA_1,choix='var',axes = c(1,2))
plot_PCA_1_var_axes1_2

# Axes factoriels 2 et 3:
plot_PCA_1_var_axes2_3 <- plot.PCA(res.PCA_1,choix='var',axes = c(2,3))
plot_PCA_1_var_axes2_3

# c) Nuage des individus:

# Axes factoriels 1 et 2:
plot_PCA_1_ind_axes1_2 <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2))
plot_PCA_1_ind_axes1_2

# Axes factoriels 2 et 3:
plot_PCA_1_ind_axes2_3 <- plot.PCA(res.PCA_1,choix='ind',axes = c(2,3))
plot_PCA_1_ind_axes2_3

# Figures complémentaires (à mettre en Annexe du mémoire?): lien avec les zonages d'étude
# Coloriage des points du nuage en fonction de la valeur des variables donnant la proportion d'atteintes
# associées à un couple de communes (I,V) présents ds un même zonage:

# même ZE:
plot_PCA_1_ind_axes1_2_ZE <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_IV_1ZE')
plot_PCA_1_ind_axes1_2_ZE
# même BV:
plot_PCA_1_ind_axes1_2_BV <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_IV_1BV')
plot_PCA_1_ind_axes1_2_BV
# même GD:
plot_PCA_1_ind_axes1_2_GD <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_IV_1GD')
plot_PCA_1_ind_axes1_2_GD
# même UU:
plot_PCA_1_ind_axes1_2_UU <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_IV_1UU')
plot_PCA_1_ind_axes1_2_UU
# même AAV:
plot_PCA_1_ind_axes1_2_AAV <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_IV_1AAV')
plot_PCA_1_ind_axes1_2_AAV
# même CENTR:
plot_PCA_1_ind_axes1_2_CENTR <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_IV_1CENTR')
plot_PCA_1_ind_axes1_2_CENTR

# même ZE:
plot_PCA_1_ind_axes1_2_IVM_ZE <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1ZE')
plot_PCA_1_ind_axes1_2_IVM_ZE
# même BV:
plot_PCA_1_ind_axes1_2_IVM_BV <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1BV')
plot_PCA_1_ind_axes1_2_IVM_BV
# même GD:
plot_PCA_1_ind_axes1_2_IVM_GD <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1GD')
plot_PCA_1_ind_axes1_2_IVM_GD
# même UU:
plot_PCA_1_ind_axes1_2_IVM_UU <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1UU')
plot_PCA_1_ind_axes1_2_IVM_UU
# même AAV:
plot_PCA_1_ind_axes1_2_IVM_AAV <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1AAV')
plot_PCA_1_ind_axes1_2_IVM_AAV
# même CENTR:
plot_PCA_1_ind_axes1_2_IVM_CENTR <- plot.PCA(res.PCA_1,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1CENTR')
plot_PCA_1_ind_axes1_2_IVM_CENTR


# B) CAH n°1: clustering des départements à partir des axes factoriels de l'ACP n°1.

# On paramètre la CAH pour déterminer 5 classes d'individus distinces (décision prise au vue de la
# forme du nuage dans les deux premières dimensions de l'ACP).

# On refait l'ACP en ne retenant que 4 axes factoriels:
res.PCA_1_4axes_facto<-PCA(data.ACP_1,ncp=4,quanti.sup=17:30,graph=FALSE)

# Réalisation de la CAH:
res.CAH_1 <- HCPC(res.PCA_1_4axes_facto, graph = TRUE,nb.clust = -1)
# On laisse l'algo choisir le nombre optimal de classes -> 3 classes sont retenues


# Choix du nombre de clusters selon le critère de la perte d'inertie:
plot_CAH_1_gains_inertie <- plot.HCPC(res.CAH_1, choice = "bar")
plot_CAH_1_gains_inertie

res.CAH_1_4clusters <- HCPC(res.PCA_1, graph = TRUE,nb.clust = 4)



# 1) Représentation du dendrogramme:

plot_CAH_1_dendrogramme <- plot.HCPC(res.CAH_1, choice = "tree")
plot_CAH_1_dendrogramme

# avec coloration des classes (optionnel!):
plot_CAH_1_dendrogramme2 <- fviz_dend(res.CAH_1,
          palette = "jco",
          main = "Regroupements des classes",
          ylab = "Inertie") 
plot_CAH_1_dendrogramme2

# 2) Projection des clusters sur le plan factoriel (axes 1 et 2):
plot(res.CAH_1, choice = "map")

plot_CAH_1_clusters_axes1_2 <- fviz_cluster(res.CAH_1,
                             repel = TRUE,            # Evite le chevauchement des textes
                             show.clust.cent = TRUE,  # Montre le centre des clusters
                             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
                             ggtheme = theme_minimal(),
                             main = "1er plan factoriel"
)
plot_CAH_1_clusters_axes1_2

# 3) Représentation en 3 dimensions du dendrogramme:
plot_CAH_1_arbre3D <- plot(res.CAH_1, choice = "3D.map")

# on récupère les clusters pour chaque observation (=département)
data_clusters_CAH_1 <-as_tibble(res.CAH_1$data.clust$clust) %>% rename(cluster=value)

# on raccroche ces clusters à la base de données ayant servi à l'ACP n°1:
dep_CAH_1 <- delinquance_dep %>% filter (is.na(DEP_inf)==FALSE & !(DEP_inf %in% c("971","972","973","974","976"))) %>%
  select(DEP_inf) 

dep.CAH_1_clusterises <- cbind(dep_CAH_1,data_clusters_CAH_1)

# Tableau 8: Dans chaque cluster de départements, part moyenne d'atteintes associées à un couple (I,V) de communes
# situées dans un même zonage d'étude (une colonne = type de zonage):

data_tableau8_CAH1 <- delinquance_dep %>% filter (is.na(DEP_inf)==FALSE & !(DEP_inf %in% c("971","972","973","974","976"))) %>%
  select(DEP_inf,Nb_I_IV_1COM,Nb_I_IV_1ZE,Nb_I_IV_1BV,Nb_I_IV_1GD,Nb_I_IV_1UU,Nb_I_IV_1AAV,Nb_I_IV_1CENTR,
         Nb_I,Nb_I_corpo,Nb_I_corpo_IVM_1COM,Nb_I_corpo_IVM_1ZE,Nb_I_corpo_IVM_1BV,Nb_I_corpo_IVM_1GD,
         Nb_I_corpo_IVM_1UU,Nb_I_corpo_IVM_1AAV,Nb_I_corpo_IVM_1CENTR) %>%
  left_join(y = dep.CAH_1_clusterises, 
            by = c("DEP_inf" = "DEP_inf"))

tableau8_CAH1 <- data_tableau8_CAH1 %>% select(-DEP_inf) %>% mutate(compteur=1) %>%
  group_by(cluster) %>%
  summarise_all(sum,na.rm=TRUE) %>% rename(nb_dep = compteur) %>%
  mutate(P_I_IV_1COM = round(Nb_I_IV_1COM/Nb_I*100,1),
         P_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I*100,1),
         P_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I*100,1),
         P_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I*100,1),
         P_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I*100,1),
         P_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I*100,1),
         P_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I*100,1),
         P_I_corpo_IVM_1COM = round(Nb_I_corpo_IVM_1COM/Nb_I_corpo*100,1),
         P_I_corpo_IVM_1ZE = round(Nb_I_corpo_IVM_1ZE/Nb_I_corpo*100,1),
         P_I_corpo_IVM_1BV = round(Nb_I_corpo_IVM_1BV/Nb_I_corpo*100,1),
         P_I_corpo_IVM_1GD = round(Nb_I_corpo_IVM_1GD/Nb_I_corpo*100,1),
         P_I_corpo_IVM_1UU = round(Nb_I_corpo_IVM_1UU/Nb_I_corpo*100,1),
         P_I_corpo_IVM_1AAV = round(Nb_I_corpo_IVM_1AAV/Nb_I_corpo*100,1),
         P_I_corpo_IVM_1CENTR = round(Nb_I_corpo_IVM_1CENTR/Nb_I_corpo*100,1)
  ) %>%
  select(cluster,nb_dep,starts_with("P_I"))

tableau8_CAH1
# Cartographie des départements clusterisés:
# TODO!
# @Clémence: en attendant, on peut utiliser ton super logiciel de carto ;)
data_carto_clusters_dep_CAH1 <- data_tableau8_CAH1 %>% select(DEP_inf,cluster)
write.csv(data_carto_clusters_dep_CAH1, "data_carto_clusters_dep_CAH1.csv", row.names=FALSE)

# données pour générer une carte sous R à partir du contour des départements:
data_carto_clusters_dep_CAH1_2 <- data_carto_clusters_dep_CAH1 %>% 
  left_join(y=contours_dep,
            by = c("DEP_inf" = "codeDep"))


# B) ACP n°2: individus = 17 306 communes de la France métropolitaine.
# 16 variables actives: volume de la délinquance (8 types d'atteinte) + structure de la délinquance (8 types d'atteinte)
# 14 variables quantitatives illustratives: 7 (IV ds même commune/ zonage d'étude) + 7 (corpo_IVM ds commune/ même zonage d'étude)

# Tableau de données de l'ACP n°2:


data.ACP_2 <- delinquance_com %>% filter (is.na(cog_com_22_inf)==FALSE &
                                            !(DEP_inf %in% c("971","972","973","974","976")) &
                                          P19_POP>0 & Nb_I_cambr>0 & Nb_I_destr_degrad>0 &
                                          Nb_I_vols_vehic>0 & Nb_I_vols_sansviol>0 &
                                            Nb_I_vols_violents>0 & Nb_I_viol_sex>0 &
                                            Nb_I_bless_famil>0 & Nb_I_bless_hfamil_homic>0) %>%
  select(cog_com_22_inf,
         # I. 16 variables actives:
         # a) 8 variables actives mesurant le volume de délinquance (pour 1000 hbts) pour chacune des 8 types 
         # d'atteinte:
         I_cambr,I_destr_degrad,I_vols_vehic,I_vols_sansviolence,I_vols_violents,I_viol_sex,I_bless_famil,
         I_bless_hfamil_homic,  
         # b) 8 variables actives mesurant la part (en %) de chaque type d'atteinte dans chaque département 
         # (structure de la délinquance par type d'atteinte):
         P_I_cambr,P_I_destr_degrad,P_I_vols_vehic,P_I_vols_sansviolence,P_I_vols_violents,P_I_viol_sex,
         P_I_bless_famil,P_I_bless_hfamil_homic,
         #II. variables qualitatives:
         #a) La part d'atteintes associées à un couple de communes (I,V) dans le même zonage:
         P_I_IV_1COM,P_I_IV_1ZE,P_I_IV_1BV,P_I_IV_1GD,P_I_IV_1UU,P_I_IV_1AAV,P_I_IV_1CENTR,
         # b) La part d'atteintes corporelles associées à un triplet de communes (I,V,M) dans le même zonage:
         P_I_corpo_IVM_1COM,P_I_corpo_IVM_1ZE,P_I_corpo_IVM_1BV,P_I_corpo_IVM_1GD,P_I_corpo_IVM_1UU,P_I_corpo_IVM_1AAV,P_I_corpo_IVM_1CENTR
         # # c) Indicateurs démographiques:
         # densite_pop,P_pop15_29ans, # calculer la part de communes classées dans la GD en "Urbain dense" (proxy d'un niveau d'urbanisation du département)
         # # d) Indicateurs socio-économiques:
         # part_chomeurs,MED20,R_P90_P10_MED20,part_pop_com_fragiles, 
         # # e) Indicateurs caractérisant les zones touristiques:
         # P_res_tourisme,P_camping_1000hbt,P_hotel,part_res_secondaires
  ) %>%
  column_to_rownames(var="cog_com_22_inf") %>% drop_na() 

# On effectue l'ACP sur ce jeu de données:

res.PCA_2<-PCA(data.ACP_2,quanti.sup = 17:30,graph=TRUE)

# Sortie d'un rapport automatique:
# Investigate(res.PCA_2)

# Exploration/visualisation des résultats: à utiliser pour explorer les résultats/visualisations alternatives

#explor(res.PCA_2)

#res.PCA_2_shiny = PCAshiny(data.ACP_2)

# Valorisation des sorties de l'ACP dans le Mémoire
# (pour de jolies sorties, priviligiez le package factoextra)
# voir la page web très bien faite à ce sujet:
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/#valeurs-propres-variances

# 2) Les figures à retenir pour le corps principale du texte du mémoire:

# a) Barplot des % d'inertie expliquée par chaque axe factoriel:
scree.plot_ACP_2 <-fviz_eig(res.PCA_2, addlabels = TRUE, ylim = c(0, 50))
scree.plot_ACP_2

# b) Cercle de corrélation des variables (actives + supplémentaires):

# Axes factoriels 1 et 2:
plot_PCA_2_var_axes1_2 <- plot.PCA(res.PCA_2,choix='var',axes = c(1,2))
plot_PCA_2_var_axes1_2

# Axes factoriels 2 et 3:
plot_PCA_2_var_axes2_3 <- plot.PCA(res.PCA_2,choix='var',axes = c(2,3))
plot_PCA_2_var_axes2_3

# c) Nuage des individus:

# Axes factoriels 1 et 2:
plot_PCA_2_ind_axes1_2 <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2))
plot_PCA_2_ind_axes1_2

# Axes factoriels 2 et 3:
plot_PCA_2_ind_axes2_3 <- plot.PCA(res.PCA_2,choix='ind',axes = c(2,3))
plot_PCA_2_ind_axes2_3

# Figures complémentaires (à mettre en Annexe du mémoire?): lien avec les zonages d'étude
# Coloriage des points du nuage en fonction de la valeur des variables donnant la proportion d'atteintes
# associées à un couple de communes (I,V) présents ds un même zonage:

# même ZE:
plot_PCA_2_ind_axes1_2_ZE <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_IV_1ZE')
plot_PCA_2_ind_axes1_2_ZE
# même BV:
plot_PCA_2_ind_axes1_2_BV <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_IV_1BV')
plot_PCA_2_ind_axes1_2_BV
# même GD:
plot_PCA_2_ind_axes1_2_GD <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_IV_1GD')
plot_PCA_2_ind_axes1_2_GD
# même UU:
plot_PCA_2_ind_axes1_2_UU <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_IV_1UU')
plot_PCA_2_ind_axes1_2_UU
# même AAV:
plot_PCA_2_ind_axes1_2_AAV <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_IV_1AAV')
plot_PCA_2_ind_axes1_2_AAV
# même CENTR:
plot_PCA_2_ind_axes1_2_CENTR <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_IV_1CENTR')
plot_PCA_2_ind_axes1_2_CENTR

# même ZE:
plot_PCA_2_ind_axes1_2_IVM_ZE <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1ZE')
plot_PCA_2_ind_axes1_2_IVM_ZE
# même BV:
plot_PCA_2_ind_axes1_2_IVM_BV <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1BV')
plot_PCA_2_ind_axes1_2_IVM_BV
# même GD:
plot_PCA_2_ind_axes1_2_IVM_GD <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1GD')
plot_PCA_2_ind_axes1_2_IVM_GD
# même UU:
plot_PCA_2_ind_axes1_2_IVM_UU <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1UU')
plot_PCA_2_ind_axes1_2_IVM_UU
# même AAV:
plot_PCA_2_ind_axes1_2_IVM_AAV <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1AAV')
plot_PCA_2_ind_axes1_2_IVM_AAV
# même CENTR:
plot_PCA_2_ind_axes1_2_IVM_CENTR <- plot.PCA(res.PCA_2,choix='ind',axes = c(1,2),habillage='P_I_corpo_IVM_1CENTR')
plot_PCA_2_ind_axes1_2_IVM_CENTR


# B) CAH n°1: clustering des départements à partir des axes factoriels de l'ACP n°1.

# On paramètre la CAH pour déterminer 5 classes d'individus distinces (décision prise au vue de la
# forme du nuage dans les deux premières dimensions de l'ACP).

# # On refait l'ACP en ne retenant que 4 axes factoriels:
# res.PCA_2_4axes_facto<-PCA(data.ACP_2,ncp=4,quanti.sup=17:30,graph=FALSE)
# 
# # Réalisation de la CAH:
# res.CAH_2 <- HCPC(res.PCA_2_4axes_facto,kk=100,description = FALSE, graph = FALSE)
# # classification avec pré-traitement par k-means avec kk=100 classes
# plot.HCPC(res.CAH_2, choice = "tree")
# 
# # Choix du nombre de clusters selon le critère de la perte d'inertie:
# plot_CAH_2_gains_inertie <- plot.HCPC(res.CAH_2, choice = "bar")
# plot_CAH_2_gains_inertie
# # 3 clusters (sur le critère de la perte d'inertie induite par l'ajout d'un cluster)
# 
# 
# # 1) Représentation du dendrogramme:
# 
# plot_CAH_2_dendrogramme <- plot.HCPC(res.CAH_2, choice = "tree")
# plot_CAH_2_dendrogramme
# 
# # avec coloration des classes (optionnel!):
# plot_CAH_2_dendrogramme2 <- fviz_dend(res.CAH_2,
#                                       palette = "jco",
#                                       main = "Regroupements des classes",
#                                       ylab = "Inertie") 
# plot_CAH_2_dendrogramme2
# 
# # 2) Projection des clusters sur le plan factoriel (axes 1 et 2):
# plot(res.CAH_2, choice = "map")
# 
# plot_CAH_2_clusters_axes1_2 <- fviz_cluster(res.CAH_2,
#                                             repel = TRUE,            # Evite le chevauchement des textes
#                                             show.clust.cent = TRUE,  # Montre le centre des clusters
#                                             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
#                                             ggtheme = theme_minimal(),
#                                             main = "1er plan factoriel"
# )
# plot_CAH_2_clusters_axes1_2
# 
# # 3) Représentation en 3 dimensions du dendrogramme:
# plot_CAH_2_arbre3D <- plot(res.CAH_2, choice = "3D.map")
# 
# # on récupère les clusters pour chaque observation (=département)
# data_clusters_CAH_2 <-as_tibble(res.CAH_2$data.clust$clust) %>% rename(cluster=value)
# 
# # on raccroche ces clusters à la base de données ayant servi à l'ACP n°1:
# com_CAH_2 <- delinquance_com %>% filter (is.na(cog_com_22_inf)==FALSE &
#                                            !(DEP_inf %in% c("971","972","973","974","976")) &
#                                            P19_POP>0 & Nb_I_cambr>0 & Nb_I_destr_degrad>0 &
#                                            Nb_I_vols_vehic>0 & Nb_I_vols_sansviol>0 &
#                                            Nb_I_vols_violents>0 & Nb_I_viol_sex>0 &
#                                            Nb_I_bless_famil>0 & Nb_I_bless_hfamil_homic>0) %>%
#   select(cog_com_22_inf) 
# 
# com.CAH_2_clusterises <- cbind(com_CAH_2,data_clusters_CAH_2)
# 
# # Tableau 8: Dans chaque cluster de départements, part moyenne d'atteintes associées à un couple (I,V) de communes
# # situées dans un même zonage d'étude (une colonne = type de zonage):
# 
# data_tableau8_CAH2 <- delinquance_com %>% filter (is.na(cog_com_22_inf)==FALSE &
#                                                     !(DEP_inf %in% c("971","972","973","974","976")) &
#                                                     P19_POP>0 & Nb_I_cambr>0 & Nb_I_destr_degrad>0 &
#                                                     Nb_I_vols_vehic>0 & Nb_I_vols_sansviol>0 &
#                                                     Nb_I_vols_violents>0 & Nb_I_viol_sex>0 &
#                                                     Nb_I_bless_famil>0 & Nb_I_bless_hfamil_homic>0) %>%
#   select(cog_com_22_inf,Nb_I_IV_1COM,Nb_I_IV_1ZE,Nb_I_IV_1BV,Nb_I_IV_1GD,Nb_I_IV_1UU,Nb_I_IV_1AAV,Nb_I_IV_1CENTR,
#          Nb_I,Nb_I_corpo,Nb_I_corpo_IVM_1COM,Nb_I_corpo_IVM_1ZE,Nb_I_corpo_IVM_1BV,Nb_I_corpo_IVM_1GD,
#          Nb_I_corpo_IVM_1UU,Nb_I_corpo_IVM_1AAV,Nb_I_corpo_IVM_1CENTR) %>%
#   left_join(y = com.CAH_2_clusterises, 
#             by = c("cog_com_22_inf" = "cog_com_22_inf"))
# 
# tableau8_CAH2 <- data_tableau8_CAH2 %>% select(-cog_com_22_inf) %>% mutate(compteur=1) %>%
#   group_by(cluster) %>%
#   summarise_all(sum,na.rm=TRUE) %>% rename(nb_com = compteur) %>%
#   mutate(P_I_IV_1COM = round(Nb_I_IV_1COM/Nb_I*100,1),
#          P_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I*100,1),
#          P_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I*100,1),
#          P_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I*100,1),
#          P_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I*100,1),
#          P_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I*100,1),
#          P_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I*100,1),
#          P_I_corpo_IVM_1COM = round(Nb_I_corpo_IVM_1COM/Nb_I_corpo*100,1),
#          P_I_corpo_IVM_1ZE = round(Nb_I_corpo_IVM_1ZE/Nb_I_corpo*100,1),
#          P_I_corpo_IVM_1BV = round(Nb_I_corpo_IVM_1BV/Nb_I_corpo*100,1),
#          P_I_corpo_IVM_1GD = round(Nb_I_corpo_IVM_1GD/Nb_I_corpo*100,1),
#          P_I_corpo_IVM_1UU = round(Nb_I_corpo_IVM_1UU/Nb_I_corpo*100,1),
#          P_I_corpo_IVM_1AAV = round(Nb_I_corpo_IVM_1AAV/Nb_I_corpo*100,1),
#          P_I_corpo_IVM_1CENTR = round(Nb_I_corpo_IVM_1CENTR/Nb_I_corpo*100,1)
#          ) %>%
#   select(cluster,nb_com,starts_with("P_I"))
# 
# tableau8_CAH2
# # Cartographie des départements clusterisés:
# # TODO!
# # @Clémence: en attendant, on peut utiliser ton super logiciel de carto ;)
# data_carto_clusters_com_CAH2 <- data_tableau8_CAH2 %>% select(cog_com_22_inf,cluster)
# write.csv(data_carto_clusters_com_CAH2, "data_carto_clusters_com_CAH2.csv", row.names=FALSE)
# 
# # données pour générer une carte sous R à partir du contour des départements:
# data_carto_clusters_com_CAH2_2 <- data_carto_clusters_com_CAH2 %>% 
#   left_join(y=contours_com,
#             by = c("cog_com_22_inf" = "insee"))
# 
