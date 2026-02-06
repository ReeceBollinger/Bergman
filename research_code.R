#Research for Junstin
#install.packages("neonUtilities")
#install.packages("neonOS")
#install.packages("terra")
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
 
names(bugData)

bugs <-  bugData|>
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

invert_pres <- pres_bugs|>
  distinct(family, sites_present, .keep_all = TRUE)|>
  View()









invert_pres
#This will give us the number of individuals for each site so we can know which family we should start with


#### This is the data manipulation for the start of 2022.
#I will start by making a table that has all of what I need. Im working with the object "bugData" because it reads in the data. 

Invert_Data <- bugData|>
  as_tibble()|>
  filter(siteID %in% streamsites)|>
  group_by(family,siteID,collectDate,sizeClass)|>
  count()

Invert_Data


#I am going to try one family first and maybe see if I can then just facet wrap by family. I want to see if the one family will work first. Ill start with "Chironomidae"

chiron <- Invert_Data|>
  filter(family == "Chironomidae")|>
  group_by(siteID)
chiron




ggplot(chiron, 
       aes(x = n, 
           y = sizeClass, 
           color = siteID))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Number of indeviduals",
       y = "Size calss in mm", 
       title = "Size of indeviduals at each site")
  


#I need would like to figure out how I can put the siteID onto a continuous scale. 

#Of course I need the average body sizes and then I need there to be a continuous scale for the siteID so that it can be on the x axis. From cold to hot.I am unsure of how I can do this however. 

feild_DATA <- read.csv("data/field_data.csv")
feild_DATA
#has the feild data, and contains mat.c which is the mean ave temperature in C at each site


Aquatic_Invert <- left_join(Invert_Data, feild_DATA)
Aquatic_Invert





#attempt to do just chiron data in one graph. 

chiron_data <- Aquatic_Invert|>
  filter(family == "Chironomidae")|>
  group_by(siteID)
chiron_data

ggplot(chiron_data,
       aes(x = mat.c,
           y = sizeClass))+
  geom_point()+
  geom_smooth(method = "lm")

# i need an average of the size class per site ID. I should just do this for all of them. 

chiron_ave <- chiron_data|>
  mutate(size_average = mean(sizeClass))
chiron_data  
View(chiron_ave)


# raw axes
ggplot(chiron_ave,
       aes(x = mat.c, 
           y = size_average))+
  geom_point()+
  geom_smooth(method = "lm", 
              color = "indianred3")+
  labs( x = "Average Temperature in C",
        y = "Average size in mm",
        title = "Average size of Chironomidae
in 2022")


# log axes
ggplot(chiron_ave,
       aes(x = mat.c, 
           y = size_average))+
  geom_point()+
  geom_smooth(method = "lm", 
              color = "indianred3")+
  labs( x = "Average Temperature in C",
        y = "Average size in mm",
        title = "Average size of Chironomidae
in 2022") +
  scale_y_log10()

#This is the chironomodiae average size across the different latitudes. I going to try to get the average for all the sites now so I can attempt to make a for loop. 



ave_invert_data <- Aquatic_Invert|>
  group_by(family, siteID)|>
  mutate(average_size = mean(sizeClass))
ave_invert_data

View(ave_invert_data)


invert_fam <- sort(unique(ave_invert_data$family))


invrt_list <- lapply(invert_fam, function(i){
  ggplot(ave_invert_data[ave_invert_data$family == i, ], aes(x = mat.c, y = average_size, color = family)) +
    geom_point()+
    geom_smooth(method = "lm")+
    labs(title = "Average body size for each family 2022",
         x = "Average temperature per site in C",
         y = "Average Body size in mm")
})

i = "Chironomidae"
paste("Average body size for", i)

print(invrt_list[[24]]) 
print(invrt_list[[22]]) 

#should be chion

print(invrt_list)



####Convert to dry body mass


source("LW_coef.R")
lw <- read.csv("macro_lw_coeffs.csv")
bugs_eq <- LW_coef(x = bugData, lw_coef = lw)

# make below an object 
buggin <- bugs_eq |>
  select(siteID, collectDate, family, formula_type, a, b, sizeClass) |>
  mutate(dw = case_when(
    formula_type == 1 ~ a *sizeClass^b,
    formula_type ==2 ~ exp(a + b * log(sizeClass))
  ))
buggin

#Join with field data to get mat.c 

dw_buggin <- left_join(buggin, feild_DATA)
dw_buggin


#mutate and group to get ave

Bug_DATA <- dw_buggin|>
  group_by(family, siteID)|>
  mutate(dw_AVE = mean(dw))
View(Bug_DATA)

#me when I ball

chiron_dw <- Bug_DATA|>
  filter(family == "Chironomidae")|>
  group_by(siteID)
chiron_dw

#graphing chirons no log

ggplot(chiron_dw,
       aes(x = mat.c,
           y = dw_AVE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Average Temp in C", 
       y = "Average Dry weight in mg",
       title = "Average DW across temperature of Chironomidae")

#Graphing with log to see the difference 

ggplot(chiron_dw,
       aes(x = mat.c,
           y = dw_AVE))+
  geom_point()+
  geom_smooth(method = "lm", 
              color = "pink")+
  labs(x = "Average Temp in C", 
       y = "Average Dry weight in mg",
       title = "Average DW across temperature of Chironomidae")+
  scale_y_log10()+
  scale_x_log10()


#function to graph all 


DW_fam <- sort(unique(Bug_DATA$family))


invrt_dw_list <- lapply(DW_fam, function(i){
  ggplot(Bug_DATA[Bug_DATA$family == i, ], aes(x = mat.c, y = dw_AVE, color = family)) +
    geom_point()+
    geom_smooth(method = "lm")+
    labs(title = "Average body size for each family 2022",
         x = "Average temperature per site in C",
         y = "Average dry weight size in mg")+
    scale_x_log10()+
    scale_y_log10()
})

print(invrt_dw_list[[26]]) 



print(invrt_dw_list)

#pres_bugs has all of the family and the different sites they are present. 
#so we should determaine what ones we would like to look at based on how often they appear. 
DW_fam




#Ceratopogonidae = 23 sites
print(invrt_dw_list[[23]]) 

#Chironomidae = 23 sites
print(invrt_dw_list[[26]]) 

#Baetidae = 22 sites
print(invrt_dw_list[[13]]) 

#Empididae = 22 sites 
print(invrt_dw_list[[46]]) 

#Enchytraeidae = 22 sites
print(invrt_dw_list[[47]]) 

#Naididae = 22 sites
print(invrt_dw_list[[100]]) 
#### no strong correlation


#Simuliidae = 22 sites
print(invrt_dw_list[[136]])

#Hygrobatidae = 21 sites
print(invrt_dw_list[[77]]) 
#### no strong correlation


#Tipulidae = 21 sites 
print(invrt_dw_list[[147]]) 
#### not a strong correlation


#Aturidae = 20 sites
print(invrt_dw_list[[12]]) 

#











