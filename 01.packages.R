## Installation
list_packages <- c("tidyverse", "readxl", "ggplot2", "data.table", "dplyr", "stargazer", "cartography","sf","rgdal","readr", "sp", "stringr", "questionr", "FactoMineR", "factoextra", "Factoshiny", "explor")
new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

## Chargement
library(tidyverse)
library(readxl)
library(ggplot2)
library(data.table)
library(dplyr)
library(stargazer)
library(cartography)
library(sf)
library(rgdal)
library(readr)
library(sp)
library(stringr)
library(questionr)
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(explor)