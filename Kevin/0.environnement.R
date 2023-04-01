setwd("M:/Commun/BESTRP/Territoires/Données communales")

library(tidyverse)
library(classInt)
library(questionr)
library(readxl)
library(cartography)
library(sf)
library(RColorBrewer)

cog.com = "CODGEO_2022"
ann.ref = "22"
var.fc = paste0("FC",ann.ref)
var.tx = paste0("tx",ann.ref)
var.tx.format = paste0(var.tx,".format")


arr.marseille = c(paste("1320",as.character(1:9),sep=""),
                  paste("132",as.character(10:16),sep=""))
arr.lyon = paste("6938",as.character(1:9),sep="")
arr.paris = paste("75",as.character(101:120),sep="")
