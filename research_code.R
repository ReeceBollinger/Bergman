#Research for Junstin
install.packages("neonUtilities")
install.packages("neonOS")
install.packages("terra")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(neonUtilities)
library(neonOS)
library(terra)

dpID="DP1.20120.001"

macro <- loadByProduct(dpID = "DP1.20120.001",
                       startdate = "2022-01",
                       enddate = "2022-12",
                       check.size = FALSE,
                       nCores = 3)
macro

neon_download(products="dpID=DP1.20120.001",
              start_date = 2022,
              end_date = 2022)


####
#Data manipulation on other script

str(macro)
macrodata <- readRDS("data/BergMacros.RDS")

saveRDS(macro, "data/BergMacros.RDS")

inv_taxonomyProcessed

streamsites=c("HOPB", "LEWI", "POSE", "CUPE",
              "GUIL", "KING", "MCDI", "LECO",
              "WALK", "MAYF", "ARIK", "BLUE",
              "PRIN", "BLDE", "COMO", "WLOU", 
              "SYCA", "REDB", "MART", "MCRA",
              "BIGC", "TECR", "OKSR", "CARI")

#Chironomidae
#this is to check if there is the taxa inside of the data. 

macro$inv_taxonomyProcessed|>
  as_tibble()|>
  filter(family == "Chironomidae",
         siteID %in% streamsites)|>
  group_by(siteID)|>
  count()|>
  View()







