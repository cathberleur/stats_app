#### Statistiques descriptives

### Statistiques descriptives sur les actes de d�linquance

## Tableau 1 : Pourcentages d'acte de d�linquance par type de d�linquance et ann�e
tab1 <- table(t.del$classe, t.del$annee)
prop.table(tab1)*100
cprop(tab1) 

## Tableau 2 : Pourcentages d'actes de d�linquance par type et d�partement
tab2 <- table(t.del$dept_serv, t.del$classe)
lprop(tab2)

## Tableau 4 : Pourcentages d'actes par classe et densit�
tab4 <- table(df_dens$classe, df_dens$gri.den)
lprop(tab4)

## Histogramme
barplot(table(t.del$classe), col = "#1b98e0", main = "Effectifs par classe de d�linquance", xlab="Classe de d�linquance", ylab = "Effectifs")

## compte le nombre de communes non renseign� pour les 3 adresses

# length((t.del$cog_com_22_inf))
# length(t.del$cog_com_22_vict)
# values(t.del$cog_com_22_mec)
# table(t.del$cog_com_22_inf)
# length(t.del[is.na(t.del$cog_com_22_inf)==TRUE,])
# length(t.del[is.na(t.del$cog_com_22_vict)==TRUE,])
# length(t.del[is.na(t.del$cog_com_22_mec),])

#sum(is.na(t.del$cog_com_22_inf))
#sum(is.na(t.del$cog_com_22_vict))
#sum(is.na(t.del$cog_com_22_mec))

cat( 'Il y a ', sum(is.na(t.del$cog_com_22_inf)), ' communes non renseign�es dans la colonne infraction. (', (sum(is.na(t.del$cog_com_22_inf))/length(t.del$cog_com_22_inf))*100,'%) \n')
cat( 'Il y a ', sum(is.na(t.del$cog_com_22_vict)), ' communes non renseign�es dans la colonne victime. (',(sum(is.na(t.del$cog_com_22_vict))/length(t.del$cog_com_22_vict))*100 ,'%) \n')
cat( 'Il y a ', sum(is.na(t.del$cog_com_22_mec)), ' communes non renseign�es dans la colonne mise en cause. (', (sum(is.na(t.del$cog_com_22_mec))/length(t.del$cog_com_22_mec))*100,'%) \n')

## Tests d'ind�pendance par r�gion/zonages
df <- merge(x=t.del, y=zonages, by.x='cog_com_22_inf', by.y='CODGEO', all.x=TRUE)
# R�gions :
tab3 <- table(df$classe, df$REG)
chisq.test(tab3) #p-value < 2.2e-16
# Les deux variables ne sont pas ind�pendantes, on rejette l'hypoth�se nulle � tous les niveaux
cramer.v(tab3) # 0.08156744
# La relation est relativement faible car proche de 0.

# Grille de densit�
df_dens <- merge(x=t.del, y=tab.dens, by.x='cog_com_22_inf', by.y='code commune', all.x=TRUE)
chisq.test(tab4) #p-value < 2.2e-16, significatif !
cramer.v(tab4) # 0.1205572
#La relation est ici plus forte qu'avec les r�gions (c'�tait attendu)

#TUU2017
tab7 <- table(df$classe, df$TUU2017)
chisq.test(tab7) #p-value < 2.2e-16
cramer.v(tab7) # 0.08504377

#pour les autres zonages, ils sont trop fins pour ce genre de tests

### Statistiques descriptives sur les zonages

## variances inter/intra

