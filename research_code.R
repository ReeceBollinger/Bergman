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
macrodata
class(macrodata)


saveRDS(macro, "data/BergMacros.RDS")

inv_taxonomyProcessed

streamsites=c("HOPB", "LEWI", "POSE", "CUPE",
              "GUIL", "KING", "MCDI", "LECO",
              "WALK", "MAYF", "ARIK", "BLUE",
              "PRIN", "BLDE", "COMO", "WLOU", 
              "SYCA", "REDB", "MART", "MCRA",
              "BIGC", "TECR", "OKSR", "CARI")

fieldData = macro$inv_fieldData
fieldData

field = fieldData %>%
  select(siteID, sampleID, benthicArea) %>%
  distinct()
field

bugData = macro$inv_taxonomyProcessed
bugData

bugs = bugData|>
  select(siteID, family)
bugs


unique(bugs$family)


#Chironomidae
#this is to check if there is the taxa inside of the data. 

macro$inv_taxonomyProcessed|>
  as_tibble()|>
  filter(family == "Chironomidae",
         siteID %in% streamsites)|>
  group_by(siteID)|>
  count()|>
  View()

#FOUND IN 23 SITES

macro$inv_taxonomyProcessed|>
  as_tibble()|>
  filter(family == "Tipulidae",
         siteID %in% streamsites)|>
  group_by(siteID)|>
  count()|>
  View()

#FOUND IN 21 SITES

#I am going to try to make a function that will list and determine if the families are found in the various different sites. 

bugs|>
  as_tibble()|>
  filter(family == "Baetidae",
         siteID %in% streamsites)|>
  group_by(siteID)|>
  count()|>
  View()

bugsite <- bugs|>
  as_tibble()|>
  filter(siteID %in% streamsites)|>
  group_by(family,siteID)|>
  count()
bugsite

##this is showing that there are ___ number of a family found at a site. 


# i would like to make another table of these result but then I want to have a column that counts the number of site that they appear in for example for the first family  Aeolosomatidae i want another column that says like site n and then lists 11. 

# n_bugs <- bugsite|>
#   as_tibble()|>
#   mutate( sites_present = n())|>
#   group_by(family, siteID)|>
#   count()


#	Aeolosomatidae = 11 sites
#Aeshnidae = 12



pres_bugs <- bugsite|>
  as_tibble()|>
  group_by(family)|>
  mutate(sites_present = n())
pres_bugs

##this is giving the number of sites that the family is found. It does not take into account the amount of individuals just counts the number of times it appears in a site.


View(pres_bugs)
#swag money

#Lets make a table of the family and the sites_present

pres_bugs|>
  distinct(family, sites_present, .keep_all = TRUE)|>
  View()

#What is the NA?



