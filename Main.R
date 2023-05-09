
# Programme principal qui génère la totalité du code R relatif au Mémoire "Les zonages d'étude au travers de la délinquance"
# (Projet Stat'app 2023 - ENSAE 2A)

# Auteurs: Catherine Berleur, Clémence Bracq, Marie Meyer et Gabriel Sklénard.

# IMPORTANT: 2 paramètres à modifier par chaque utilisateur avant d'excécuter ce programme:

# 1) Votre WD:
setwd("/Users/sklenard/Documents/Statapp/WD_Gabriel") # WD pour Gabriel.
# c'est à cet endroit que devront être enregistrés les différents fichiers servant  d'"inputs" pour notre travail
# (cf. la liste juste rappelée ci-dessous)

# 2) Préciser le chemin pointant vers le dossier où sont enregistrés les 8 scripts R: le Main.R + les 7 scripts associés:
scripts_path="/Users/sklenard/Documents/GitHub/stats_app" # scripts_path pour Gabriel.

# Remarque pour Clémence, Marie et Catherine: pour nous le chemin doit pointer vers notre repo Github créé sur notre ordi
# perso via Desktop, ce qui permet de toujours pointer sur la dernière version validée Github des scripts ;)

# WARNING: il faut penser à enregistrer sous votre WD les 9 fichiers suivants:
# 1) le fichier donnees.secretisees.delinquance.RData (version secrétisée de la base géolocalisée des atteintes du SSMSI);
# @Kevin et Aurélien: enregistrez la version non secrétisée du fichier ;)
# 2) le fichier table-appartenance-geo-communes-23.xlsx (source: Insee https://www.insee.fr/fr/information/2028028);
# 3) le fichier BV2022_au_01-01-2022.xlsx (source: https://www.insee.fr/fr/information/6676988);
# 4) le fichier UU2020_au_01-01-2023.xlsx (source: https://www.insee.fr/fr/information/4802589);
# 5) le fichier grille.densité.rdata (mis à disposition par le SSMSI sur OSMOSE);
# 6) le fichier 202009_data_etudescentralites_inrae_anct.xlsx (mis à disposition sur OSMOSE);
# 7) le fichier base_cc_comparateur.xlsx (source: Insee https://www.insee.fr/fr/statistiques/2521169);
# 8) les fichiers dossier_complet.csv et meta_dossier_complet.csv (source: Insee https://www.insee.fr/fr/statistiques/5359146);
# 9) le fichier donnees.secretisees.delinquance.distances.rdata (version secrétisée du fichier des distances entre communes
# de l'infraction, de la victime et du mis en cause  (calculs SSMSI)) ;
# @Kevin et Aurélien: enregistrez la version non secrétisée du fichier ;)

# IMPORTANT!!
# @ Kevin et Aurélien: 
# les seules micro-modifs dans nos scripts R fournis à faire se situent uniquement dans le programme "1_Chargement_data.R";
# Elles consistent à simplement remplacer les deux fichiers secrétisés;
# (i) donnees.secretisees.delinquance.distances.rdata
# (ii) donnees.secretisees.delinquance.RData) 
# par leur version non secrétisée stockée au SSMSI sous le répertoire M...

# Etape 0 - Les packages utilisés dans ce projet:
source(paste0(scripts_path,"/0_Chargement_packages.R"))
# @Kevin et Aurélien: RAS (rien à modifier ici!)


# Etape 1 - Le chargement des différentes sources de données utilisées dans ce projet:
source(paste0(scripts_path,"/1_Chargement_data.R"))
# IMPORTANT! @Kevin et Aurélien: 
# Pensez ici à remplacer les deux fichiers .Rdata secréitisés chargés dans ce script
# par leur version non secrétisée !!


# Etape 2: La construction des bases de données analysées dans ce projet:
source(paste0(scripts_path,"/2_Construction_bases_etude.R"))
# @Kevin et Aurélien: 1 seule chose à modifier dans ce script -> commenter le petit morceau de code 
# qui impute des modalités explicites à la variable "classe" (à la place des lettres).
# Il s'agit des lignes 145 à 158.
# Cette modif est très importante, car potentiellement on a pu faire des mauvais reclassements!


# Etape 3: Partie 1 du mémoire: Les données
source(paste0(scripts_path,"/3_Partie1_Memoire.R"))
# @Kevin et Aurélien: RAS (rien à modifier ici!)

# Tableau 1: Les différents types d'atteinte selon l'information spatiale disponible
write.csv(tableau1, "Tableau1.csv", row.names=FALSE)


# Etape 4: Partie 2 du mémoire: Faits stylisés: une description fine de l'organisation spatiale de la délinquance
source(paste0(scripts_path,"/4_Partie2_Memoire.R"))
# @Kevin et Aurélien: RAS (rien à modifier ici!)

# Tableau 2: Distribution du nombre d'infractions au niveau communal (pour 1000 habitants), selon le type d'atteinte
write.csv(tableau2, "Tableau2.csv", row.names=FALSE)

# Graphique 1: Nombre d’infractions par commune (en log) en fonction de leur taille (en log)
png("Graphique1.png")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I,main = "", xlab="Nombre d'habitants (log)", ylab="Nombre d'infractions (log)")
dev.off()

# Détail par type d'atteinte en Annexe:
png("Graphique1bis_annexe.png")
layout(matrix(1:6,2,3))
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_cambr,main = "Cambriolages", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_bless_famil,main = "Blessures intra-familiales", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_bless_horsfamil,main = "Blessures extra-familiales", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_destr_degrad,main = "Destructions, dégradations", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_homic,main = "Homicides", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_viol_sex,main = "Violences sexuelles", xlab="Pop (log)", ylab="Nb I (log)")
dev.off()

png("Graphique1ter_annexe.png")
layout(matrix(1:6,2,3))
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_armes,main = "Violences avec armes", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_acces_vehic,main = "Vols d'accessoires de véhicules", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_ds_vehic,main = "Vols dans les véhicules", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_de_vehic,main = "Vols de véhicules", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_sansviol,main = "Vol sans violence", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_viol_sansarme,main = "Vol violents sans armes", xlab="Pop (log)", ylab="Nb I (log)")
dev.off()

# Graphique 2: matrice de corrélation des nombres d'infractions par commune (pour 1000 habitants) selon le type d'atteinte
png("Graphique2.png")
corrplot(data.graphique2.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)
dev.off()

# Graphiques 2a et 2b ci-desspis à mettre en Annexe (ou à supprimer du Mémoire):

# Graphique 2a (Annexe): Matrice de corrélation des nombres d'infractions (I) et des nombres de victimes (V),pour 1000 habitants 
# - par type d'atteinte
png("Graphique2a_annexe.png")
corrplot(data.graphique2a_annexe.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)
dev.off()

# Graphique 2b (Annexe): Matrice de corrélation des nombres d'infractions (I), des nombres de victimes (V), et des nombres de 
# mis en cause, pour 1000 habitants - par type d'atteinte
png("Graphique2b_annexe.png")
corrplot(data.graphique2b_annexe.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)
dev.off()


# Graphique 3a: Distribution de la différence des log du nombre d'infractions et du nombre de victimes 
# mesurée dans chaque commune (pour 1000 habitants) - 
# Note: seules les atteintes non corporelles sont considérées ici!
png("Graphique3a.png")
ggplot(data.graphique3a, aes(x = type_atteinte, y = dlog_I_V)) + geom_boxplot()
dev.off()

# Graphique 3b: 
# 1- Distribution de la différence des log du nombre d'infractions et du nombre de victimes
# mesurée dans chaque commune (pour 1000 habitants) 
# 2- Distribution de la différence des log du nombre d'infractions et du nombre de mis en cause 
# mesurée dans chaque commune (pour 1000 habitants) 
# Note: seules les atteintes corporelles sont considérées ici!

png("Graphique3b_1.png")
ggplot(data.graphique3b_1, aes(x = type_atteinte, y = dlog_I_V)) + geom_boxplot()
dev.off()
png("Graphique3b_2.png")
ggplot(data.graphique3b_2, aes(x = type_atteinte, y = dlog_I_M)) + geom_boxplot()
dev.off()


# Tableau 3a: Proportion d'infractions (I) associées à un couple de communes (I,V) présentes dans un même zonage d'étude
# - selon le type d'atteinte
write.csv(tableau3a, "Tableau3a.csv", row.names=FALSE)

# Tableau 3b: Proportion d'infractions (I) associées à un triplet de communes (I,V,M) présentes dans un même zonage d'étude
# - selon le type d'atteinte
write.csv(tableau3b, "Tableau3b.csv", row.names=FALSE)

# A mettre en Annexe ou dans le corps principal du mémoire:

# Tableau 3a_bis: Proportion d'infractions (I) associées à un couple de communes (I,V) présentes dans un même zonage d'étude
# - selon le type d'atteinte - après correction des différences de taille des différents zonages
write.csv(tableau3a_bis, "Tableau3a_bis.csv", row.names=FALSE)

# Tableau 3b_bis: Proportion d'infractions (I) associées à un triplet de communes (I,V,M) présentes dans un même zonage d'étude
# - selon le type d'atteinte - après correction des différences de taille des différents zonages
write.csv(tableau3b_bis, "Tableau3b_bis.csv", row.names=FALSE)


# A mettre éventuellement en Annexe, en réplique des tableaux 3a et 3b en distinguant les sous-périodes 2016-2019 (pré-Covid)
# et 2020-2021 (Covid):
write.csv(tableau3a_2016_2019_annexe, "Tableau3a_2016_2019_annexe.csv", row.names=FALSE)
write.csv(tableau3a_2020_2021_annexe, "Tableau3a_2020_2021_annexe.csv", row.names=FALSE)
write.csv(tableau3b_2016_2019_annexe, "Tableau3b_2016_2019_annexe.csv", row.names=FALSE)
write.csv(tableau3b_2020_2021_annexe, "Tableau3b_2020_2021_annexe.csv", row.names=FALSE)

# Raffinements visant à analyser le statut des communes (I,V,M) au sein d'un même zonage...

# 1) Statut des communes au sein du bassin de vie (BV):

# Tableau 4a: Répartition (en %) des atteintes associées à un couple de communes (I,V) dans un même BV, selon le statut
# respectif de la commune de I et celle de V au sein du BV. 
# Rappel: 00: "Non pôle"; 11: "Pôle partiel"; 12: "Commune associée à un pôle partiel" et 20: "Pôle".
write.csv(tableau4a, "Tableau4a.csv", row.names=FALSE)

# Tableau 4b: Répartition (en %) des atteintes associées à un triplet de communes (I,V,M) dans un même BV, selon le statut
# respectif de la commune de I, de celle de V et de celle de M au sein du BV. 
# Rappel: 00: "Non pôle"; 11: "Pôle partiel"; 12: "Commune associée à un pôle partiel" et 20: "Pôle".
# Remarque: pour construire nos modalités, on ne reprend que les modalités avec les plus grosses freq dans le tableau 4a ;)
# On se restreint ici aux seules atteintes corporelles.
write.csv(tableau4b, "Tableau4b.csv", row.names=FALSE)

# 2) Statut des communes au sein de l'unité urbaine (UU):

# Tableau 5a: Répartition (en %) des atteintes associées à un couple de communes (I,V) dans une même UU, selon le statut
# respectif de la commune de I et celle de V au sein de l'UU. 
# Rappel: H: "Hors UU"; C: "Ville-centre"; B: "Banlieue" et I: "Ville isolée".
write.csv(tableau5a, "Tableau5a.csv", row.names=FALSE)

# Tableau 5b: Répartition (en %) des atteintes associées à un triplet de communes (I,V,M) dans une même UU, selon le statut
# respectif de la commune de I, de celle de V et de celle de M au sein de l'UU. 
# Rappel: H: "Hors UU"; C: "Ville-centre"; B: "Banlieue" et I: "Ville isolée".
# Remarque: pour construire nos modalités, on ne reprend que les modalités avec les plus grosses freq dans le tableau 5a ;)
# On se restreint ici aux seules atteintes corporelles.
write.csv(tableau5b, "Tableau5b.csv", row.names=FALSE)

# 3) Statut des communes au sein de l'aire d'attraction des villes (AAV):

# Tableau 6a: Répartition (en %) des atteintes associées à un couple de communes (I,V) dans une même AAV, selon le statut
# respectif de la commune de I et celle de V au sein de l'AAV. 
# Rappel: 11: "Commune-centre"; 12: "Autre commune du pôle principal"; 13: "Commune d'un pôle secondaire";
# 20: "Commune de la couronne"; 30: "Commune hors AAV".
write.csv(tableau6a, "Tableau6a.csv", row.names=FALSE)

# Tableau 6b: Répartition (en %) des atteintes associées à un triplet de communes (I,V,M) dans une même AAV, selon le statut
# respectif de la commune de I, de celle de V et de celle de M au sein de l'AAV. 
# Rappel: 11: "Commune-centre"; 12: "Autre commune du pôle principal"; 13: "Commune d'un pôle secondaire";
# 20: "Commune de la couronne"; 30: "Commune hors AAV".
write.csv(tableau6b, "Tableau6b.csv", row.names=FALSE)

# 3) Type des communes (I,V) (au sens de la GD) qui se retrouvent dans un même zonage:

# Tableau 7a:  Répartition (en %) des atteintes associées à un couple de communes (I,V) dans un même zonage, selon le statut
# respectif de la commune de I et celle de V au sein de la GD 
# Rappel: les différentes modalités de la GD sont:
# Urbain dense (UD); Urbain densité intermédiaire (UDI); Rural périurbain (RP); Rural non périurbain (RNP)
write.csv(tableau7a, "Tableau7a.csv", row.names=FALSE)

# Tableau 7b: Répartition (en %) des atteintes associées à un triplet de communes (I,V,M) dans une même zonage, selon le statut
# respectif de la commune de I, de celle de V et de celle de M au sein de la GD. 
# Rappel: les différentes modalités de la GD sont:
# Urbain dense (UD); Urbain densité intermédiaire (UDI); Rural périurbain (RP); Rural non périurbain (RNP)
# Remarque: pour construire nos modalités, on ne reprend que les modalités avec les plus grosses freq dans le tableau 7a ;)
# On se restreint ici aux seules atteintes corporelles.
write.csv(tableau7b, "Tableau7b.csv", row.names=FALSE)

# Une variante des tableaux 7: pour chaque type d'atteinte, répartition des couples de communes (I,V) selon les différentes
# modalités de la grille de densité:

# on se concentre d'abord sur les seules atteintes non corporelles (7a_bis):
write.csv(tableau7a_bis, "Tableau7a_bis.csv", row.names=FALSE)

# puis on se concentre sur les seules atteintes corporelles (7b_bis):
write.csv(tableau7b_bis, "Tableau7b_bis.csv", row.names=FALSE)

# ultime variante sur les atteintes corporelles: on peut dupliquer le tableau 7b_bis non plus en étudiant la répartition
# selon  le couple de communes (I,V) dans la GD mais selon le couple (M,V) dans la GD:
write.csv(tableau7b_ter, "Tableau7b_ter.csv", row.names=FALSE)


# Etape 5: Partie 3 du mémoire: Classification des communes et des départements de France métropolitaine au regard de la délinquance
# source(paste0(scripts_path,"/5_Partie3_Memoire.R"))
# @Kevin et Aurélien: le code est en cours de finalisation et vous sera transmis très prochainement...


# Etape 6: Partie 4 du mémoire: Lien entre les clusters de communes et les zonages d'étude
# source(paste0(scripts_path,"/6_Partie4_Memoire.R"))
# @Kevin et Aurélien: le code est en cours de finalisation et vous sera transmis très prochainement...

