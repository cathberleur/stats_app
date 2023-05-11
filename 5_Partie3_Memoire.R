
# Partie 3 du mémoire: Classification des communes et des départements de France métropolitaine au regard de la délinquance


# A) ACP n°1: au niveau départemental 

# Tableau de données de l'ACP n°1:

data.ACP_1 <- delinquance_dep %>% filter (is.na(DEP_inf)==FALSE & !(DEP_inf %in% c("971","972","973","974","976"))) %>%
                              select(DEP_inf,
                                     # I. 30 variables actives:
                                     # a) 8 variables actives mesurant le volume de délinquance (pour 1000 hbts) pour chacune des 8 types 
                                     # d'atteinte:
                                     I_cambr,I_destr_degrad,I_vols_vehic,I_vols_sansviolence,I_vols_violents,I_viol_sex,I_bless_famil,
                                     I_bless_hfamil_homic,  
                                     # b) 8 variables actives mesurant la part (en %) de chaque type d'atteinte dans chaque département 
                                     # (structure de la délinquance par type d'atteinte):
                                     P_I_cambr,P_I_destr_degrad,P_I_vols_vehic,P_I_vols_sansviolence,P_I_vols_violents,P_I_viol_sex,
                                     P_I_bless_famil,P_I_bless_hfamil_homic,
                                     # c) 8 variables actives mesurant la distance médiane (en km) entre la commune de l'infraction (I) et 
                                     # celle de domiciliation de la victime (V) pour chacune des 8 types d'atteinte:
                                     dist_I_V_Cambr,dist_I_V_Destr_degrad,dist_I_V_Vols_vehic,dist_I_V_Vols_sansviolence,dist_I_V_Vols_violents,
                                     dist_I_V_Viol_sex,dist_I_V_Bless_famil,dist_I_V_Bless_hfamil_homic,
                                     # d) 3 variables actives mesurant la distance médiane (en km) entre la commune de l'infraction (I) et 
                                     # celle de domiciliation du mis en cause (M) pour chacune des 3 types d'atteinte corporelles:
                                     dist_I_M_Viol_sex,dist_I_M_Bless_famil,dist_I_M_Bless_hfamil_homic,
                                     # e) 3 variables actives mesurant la distance médiane (en km) entre la commune de domiciliation de la victime (V) et 
                                     # celle de domiciliation du mis en cause (M) pour chacune des 3 types d'atteinte corporelles:
                                     dist_V_M_Viol_sex,dist_V_M_Bless_famil,dist_V_M_Bless_hfamil_homic,
                                     # II. variables qualitatives:
                                     # a) La part d'atteintes associées à un couple de communes (I,V) dans le même zonage:
                                     P_I_IV_1ZE,P_I_IV_1BV,P_I_IV_1GD,P_I_IV_1UU,P_I_IV_1AAV,P_I_IV_1CENTR,
                                     # b) La part d'atteintes corporelles associées à un triplet de communes (I,V,M) dans le même zonage:
                                     P_I_corpo_IVM_1ZE,P_I_corpo_IVM_1BV,P_I_corpo_IVM_1GD,P_I_corpo_IVM_1UU,P_I_corpo_IVM_1AAV,
                                     # c) Indicateurs démographiques:
                                     densite_pop,P_pop15_29ans, # calculer la part de communes classées dans la GD en "Urbain dense" (proxy d'un niveau d'urbanisation du département)
                                     # d) Indicateurs socio-économiques:
                                     part_chomeurs,MED20,R_P90_P10_MED20,part_pop_com_fragiles, 
                                     # on pourrait aussi calculer la part de communes situées dans un centralité classées comme "fragiles" par l'Inrae?
                                     # e) Indicateurs caractérisant les zones touristiques:
                                     P_res_tourisme,P_camping_1000hbt,P_hotel,part_res_secondaires
                                     ) %>%
                              column_to_rownames(var="DEP_inf")

# On effectue l'ACP sur ce jeu de données:
res.PCA_1<-PCA(data.ACP_1,quanti.sup = 31:51,graph=TRUE)

# Exploration/visualisation des résultats: à utiliser pour explorer les résultats/visualisations alternatives

#explor(res.PCA_1)

#res.PCA_1_shiny = PCAshiny(data.ACP_1)

# Valorisation des sorties de l'ACP dans le Mémoire
# (pour de jolies sorties, priviligiez le package factoextra)
# voir la page web très bien faite à ce sujet:
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/#valeurs-propres-variances



# 1) Les sorties à retenir pour le commentaire de l'ACP dans le mémoire:
summary(res.PCA_1)
print(res.PCA_1)

# - Récupérer les valeurs propres et le graphique du % d'inertie expliquée par les axes factoriels:

eig.val <- get_eigenvalue(res.PCA_1)
eig.val

# D'après la règle de Kaiser (valeurs propres>1): on retiendrait ici les 4 premiers axes de l'ACP pour l'interprétation.

# Visualisation des valeurs propres (scree plot) et règle du coude:

scree.plot_ACP_1 <-fviz_eig(res.PCA_1, addlabels = TRUE, ylim = c(0, 50))
scree.plot_ACP_1
# On a un premier coude au niveau du 3 axe factoriel (on en garderait alors que 3 selon la règle du coude)
# mais on a un deuxième coude au niveau du 4ème axe, après lequel on a une vraie stabilisation à un niveau proche
# de zéro..., ce qui pourrait nous conduire à garder les 4 premiers axes comme pour la règle de Kaiser.
# 88% de l'inertie totale est contenue dans les 4 premiers axes, ce qui est très bien!

# TOTO: pensez à exporter le graphique des valeurs propres dans le Main.R.

# - Interprétation du nuage des variables: extraction des résultats utiles
# coordonnées, corrélations avec les axes, cos2 et contributions.

var <- get_pca_var(res.PCA_1)
var

# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

# Visualisation des variables dans le cercle de corrélation:
fviz_pca_var(res.PCA_1, col.var = "black")

# Visualisation de la qualité de représentation des variables: cos2
corrplot(var$cos2, is.corr=FALSE)
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.PCA_1, choice = "var", axes = 1:2)

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.PCA_1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

# Changer la transparence en fonction du cos2
fviz_pca_var(res.PCA_1, alpha.var = "cos2")

 # Contribution des variables aux axes principaux:
corrplot(var$contrib, is.corr=FALSE)  

# Contributions des variables à PC1:
fviz_contrib(res.PCA_1, choice = "var", axes = 1, top = 15)
# Contributions des variables à PC2:
fviz_contrib(res.PCA_1, choice = "var", axes = 2, top = 15)
# Contributions des variables à PC3:
fviz_contrib(res.PCA_1, choice = "var", axes = 3, top = 10)
# Contributions des variables à PC4:
fviz_contrib(res.PCA_1, choice = "var", axes = 4, top = 10)

# La contribution totale à PC1 et PC2:
fviz_contrib(res.PCA_1, choice = "var", axes = 1:2, top = 20)

# Graph de corrélation en fonction de la contribution des variables aux axes
fviz_pca_var(res.PCA_1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Changez la transparence en fonction de contrib
fviz_pca_var(res.PCA_1, alpha.var = "contrib")

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

# B) CAH n°1: clustering des départements à partir des axes factoriels de l'ACP n°1.

# On paramètre la CAH pour déterminer 5 classes d'individus distinces (décision prise au vue de la
# forme du nuage dans les deux premières dimensions de l'ACP).

# On refait l'ACP en ne retenant que 4 axes factoriels:
res.PCA_1<-PCA(data.ACP_1,ncp=4,quanti.sup=31:51,graph=FALSE)
# IMPORTANT: On réalise la CAH sur la base de l'ACP sur les 4 premiers facteurs (qui expliquent plus de 80% de l'inertie totale)

# Réalisation de la CAH:
res.CAH_1 <- HCPC(res.PCA_1, graph = FALSE,nb.clust = -1)
# On laisse l'algo choisir le nombre optimal de classes -> 3 classes sont retenues


# Choix du nombre de clusters selon le critère de la perte d'inertie:
plot_CAH_1_gains_inertie <- plot.HCPC(res.CAH_1, choice = "bar")
plot_CAH_1_gains_inertie
# Finalement le saut de baisse d'inertie se situe plutôt à 4 classes;
# On retient donc finalement 4 clusters
# On refait tourner la CAH en précisant 4 classes:
res.CAH_1 <- HCPC(res.PCA_1, graph = FALSE,nb.clust = 4)


# Summary de la CAH:
res.CAH_1$call

# Description des classes de la partition:
res.CAH_1$desc.va


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
plot_CAH_1_clusters_axes1_2 <- plot(res.CAH_1, choice = "map")

fviz_cluster <- fviz_cluster(res.CAH_1,
                             repel = TRUE,            # Evite le chevauchement des textes
                             show.clust.cent = TRUE,  # Montre le centre des clusters
                             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
                             ggtheme = theme_minimal(),
                             main = "1er plan factoriel"
)
fviz_cluster

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

data_tableau8 <- delinquance_dep %>% filter (is.na(DEP_inf)==FALSE & !(DEP_inf %in% c("971","972","973","974","976"))) %>%
  select(DEP_inf,Nb_I_IV_1ZE,Nb_I_IV_1BV,Nb_I_IV_1GD,Nb_I_IV_1UU,Nb_I_IV_1AAV,Nb_I_IV_1CENTR,
         Nb_I) %>%
  left_join(y = dep.CAH_1_clusterises, 
            by = c("DEP_inf" = "DEP_inf"))

tableau8 <- data_tableau8 %>% select(-DEP_inf) %>% mutate(compteur=1) %>%
  group_by(cluster) %>%
  summarise_all(sum,na.rm=TRUE) %>% rename(nb_dep = compteur) %>%
  mutate(P_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I*100,1),
         P_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I*100,1),
         P_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I*100,1),
         P_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I*100,1),
         P_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I*100,1),
         P_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I*100,1)) %>%
  select(cluster,nb_dep,starts_with("P_I"))

# Cartographie des départements clusterisés:
# TODO!
# @Clémence: en attendant, on peut utiliser ton super logiciel de carto ;)
data_carto_clusters_dep <- data_tableau8 %>% select(DEP_inf,cluster)
write.csv(data_carto_clusters_dep, "data_carto_clusters_dep.csv", row.names=FALSE)

# données pour générer une carte sous R à partir du contour des départements:
data_carto_clusters_dep2 <- data_carto_clusters_dep %>% 
  left_join(y=contours_dep,
            by = c("DEP_inf" = "codeDep"))


# B) ACP n°2 au niveau communal:

# On peut répliquer cette ACP-CAH au niveau communal, dans le but de disposer d'une classification des communes au regard
# de la délinquance mesurée dans trois dimension (spatiale, volume et structure)

data.ACP_2 <- delinquance_com %>% filter (is.na(cog_com_22_inf)==FALSE & 
                                            is.na(DEP_inf)==FALSE &
                                            !(DEP_inf %in% c("971","972","973","974","976"))) %>%
  select(cog_com_22_inf,
         # I. 30 variables actives:
         # a) 8 variables actives mesurant le volume de délinquance (pour 1000 hbts) pour chacune des 8 types 
         # d'atteinte:
         I_cambr,I_destr_degrad,I_vols_vehic,I_vols_sansviolence,I_vols_violents,I_viol_sex,I_bless_famil,
         I_bless_hfamil_homic,  
         # b) 8 variables actives mesurant la part (en %) de chaque type d'atteinte dans chaque département 
         # (structure de la délinquance par type d'atteinte):
         P_I_cambr,P_I_destr_degrad,P_I_vols_vehic,P_I_vols_sansviolence,P_I_vols_violents,P_I_viol_sex,
         P_I_bless_famil,P_I_bless_hfamil_homic,
         # c) 8 variables actives mesurant la distance médiane (en km) entre la commune de l'infraction (I) et 
         # celle de domiciliation de la victime (V) pour chacune des 8 types d'atteinte:
         dist_I_V_Cambr,dist_I_V_Destr_degrad,dist_I_V_Vols_vehic,dist_I_V_Vols_sansviolence,dist_I_V_Vols_violents,
         dist_I_V_Viol_sex,dist_I_V_Bless_famil,dist_I_V_Bless_hfamil_homic,
         # d) 3 variables actives mesurant la distance médiane (en km) entre la commune de l'infraction (I) et 
         # celle de domiciliation du mis en cause (M) pour chacune des 3 types d'atteinte corporelles:
         dist_I_M_Viol_sex,dist_I_M_Bless_famil,dist_I_M_Bless_hfamil_homic,
         # e) 3 variables actives mesurant la distance médiane (en km) entre la commune de domiciliation de la victime (V) et 
         # celle de domiciliation du mis en cause (M) pour chacune des 3 types d'atteinte corporelles:
         dist_V_M_Viol_sex,dist_V_M_Bless_famil,dist_V_M_Bless_hfamil_homic,
         # II. variables qualitatives:
         # a) La part d'atteintes associées à un couple de communes (I,V) dans le même zonage:
         P_I_IV_1ZE,P_I_IV_1BV,P_I_IV_1GD,P_I_IV_1UU,P_I_IV_1AAV,P_I_IV_1CENTR,
         # b) La part d'atteintes corporelles associées à un triplet de communes (I,V,M) dans le même zonage:
         P_I_corpo_IVM_1ZE,P_I_corpo_IVM_1BV,P_I_corpo_IVM_1GD,P_I_corpo_IVM_1UU,P_I_corpo_IVM_1AAV,
         # c) Indicateurs démographiques:
         densite_pop,P_pop15_29ans, # calculer la part de communes classées dans la GD en "Urbain dense" (proxy d'un niveau d'urbanisation du département)
         # d) Indicateurs socio-économiques:
         part_chomeurs,MED20,R_SCORE_inf, 
         # on pourrait aussi calculer la part de communes situées dans un centralité classées comme "fragiles" par l'Inrae?
         # e) Indicateurs caractérisant les zones touristiques:
         P_res_tourisme,P_camping_1000hbt,P_hotel,part_res_secondaires
  ) %>%
  drop_na() %>%
  column_to_rownames(var="cog_com_22_inf")

# On effectue l'ACP sur ce jeu de données:
res.PCA_2<-PCA(data.ACP_2,quanti.sup = 31:50,graph=TRUE)

res.PCA_2



 