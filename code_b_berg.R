#data manipulation






macrodata <- readRDS("data/BergMacros.RDS")

saveRDS(macro, "data/BergMacros.RDS")


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

#Now from here 












































