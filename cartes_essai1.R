library(sf)
library(rgdal)
library(cartography)
library(sp)

#install.packages("cartography")

load("C:\\Users\\marie\\OneDrive\\Documents\\cours\\ensae\\stat app\\donnees.secretisees.delinquance.RData")
donnees_del <- t.del
head(donnees_del)


communes <- readOGR(dsn = "C:\\Users\\marie\\OneDrive\\Documents\\cours\\ensae\\stat app\\contoursGeographiques", layer = "communes-20220101-simpl04", verbose =
                   FALSE)
class(communes)
plot(communes)

champignelles <- subset(communes, wikipedia =="fr:Champignelles")
plot(champignelles)

communes_sup <- communes[communes@data$surf_ha>200000, ]
communes_sup             
plot(communes_sup)

communes2 <- subset(communes, wikipedia %in% c("fr:Lano","fr:Cambia"))
communes2
plot(communes2)+plot(champignelles, add=TRUE)

propSymbolsLayer(spdf=communes2, df=donnees_del, spdfid = "insee",
                 dfid = "cog_com_22_inf", var= "classe", col="salmon",
                 symbols="circle", legend.pos="right")

essai
