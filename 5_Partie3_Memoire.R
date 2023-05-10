
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
                                     part_chomeurs,MED20, # rajouter un indicateur de dispersion,d'inégalité des revenus à l'échelle départementale?
                                     # on pourrait aussi calculer la part de communes situées dans un centralité classées comme "fragiles" par l'Inrae?
                                     # e) Indicateurs caractérisant les zones touristiques:
                                     P_res_tourisme,P_camping_1000hbt,P_hotel,part_res_secondaires
                                     ) %>%
                              column_to_rownames(var="DEP_inf")

# On effectue l'ACP sur ce jeu de données:
res.PCA_1<-PCA(data.ACP_1,quanti.sup = 31:49,graph=TRUE)

# Exploration/visualisation des résultats:
explor(res.PCA_1)

res.PCA_1_shiny = PCAshiny(data.ACP_1)

# Valorisation des sorties de l'ACP dans le Mémoire
# (pour de jolies sorties, priviligiez le package factoextra)
# voir la page web très bien faite à ce sujet:
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/#valeurs-propres-variances

# a) Les figures à retenir pour le mémoire:
plot.PCA(res.PCA_1,choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA_1,title="Graphe des individus de l'ACP")
# Coloriage des points du nuage en fonction de la valeur d'une variable qualitative:
plot.PCA(res.PCA,habillage='P_I_IV_1ZE',title="Graphe des individus de l'ACP")
plot.PCA(res.PCA,habillage='P_I_IV_1BV',title="Graphe des individus de l'ACP")
plot.PCA(res.PCA,habillage='P_I_IV_1GD',title="Graphe des individus de l'ACP")
plot.PCA(res.PCA,habillage='P_I_IV_1UU',title="Graphe des individus de l'ACP")
plot.PCA(res.PCA,habillage='P_I_IV_1AAV',title="Graphe des individus de l'ACP")
plot.PCA(res.PCA,habillage='P_I_IV_1CENTR',title="Graphe des individus de l'ACP")

plot.PCA(res.PCA,habillage='part_res_secondaires',title="Graphe des individus de l'ACP")


# b) Les sorties à retenir pour le commentaire de l'ACP dans le mémoire:
summary(res.PCA_1)
print(res.PCA_1)

# - Récupérer les valeurs propres et le graphique du % d'inertie expliquée par les axes factoriels:

eig.val <- get_eigenvalue(res.PCA_1)
eig.val

# D'après la règle de Kaiser (valeurs propres>1): on retiendrait ici les 4 premiers axes de l'ACP pour l'interprétation.

# Visualisation des valeurs propres (scree plot) et règle du coude:

fviz_eig(res.PCA_1, addlabels = TRUE, ylim = c(0, 50))
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

# B) CAH n°1: clustering des départements à partir des axes factoriels de l'ACP n°1.

# On paramètre la CAH pour déterminer 4 classes d'individus distinces (décision prise au vue de la
# forme du nuage dans les deux premières dimensions de l'ACP).

res.PCA_1<-PCA(data.ACP_1,ncp=4,quanti.sup=c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49),graph=FALSE)
res.HCPC_1<-HCPC(res.PCA_1,nb.clust=5,consol=TRUE,graph=FALSE)
plot.HCPC(res.HCPC_1,choice='tree',title='Arbre hiérarchique')
plot.HCPC(res.HCPC_1,choice='map',draw.tree=FALSE,title='Plan factoriel')
plot.HCPC(res.HCPC_1,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Arbre hiérarchique sur le plan factoriel')

# on récupère les clusters pour chaque observation (=département)

data_clusters <-as_tibble(res.HCPC_1$data.clust) %>% rownames_to_column(var="DEP_inf")
# attention les premiers numéros de département ont perdu leur zéro!!










 