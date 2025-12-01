library(tidyverse)
source("code/custom-functions/LW_coef.R")
coeff <- read.csv("data/raw_data/inverts/macro_lw_coeffs.csv")
# add length weight coefficients by taxon
MN.lw <- LW_coef(x = macro$inv_taxonomyProcessed,
                 lw_coef = coeff,
                 percent = TRUE)

# questionable measurements ####
# filter out individuals that were "damaged" and measurement was affected
# this is a flag which is added by NEON
# MN.no.damage <- MN.lw %>%
#   filter(!str_detect(sampleCondition,
#                      "measurement")) %>%
#   est_dw(fieldData = macro$inv_fieldData)

# length_weight estimates -------------------------------------------------



# est_dw <- function(x, fieldData){
# x = inv_taxonomyProcessed table from NEON data product "DP1.20120.001" with LW coefficients added using the LW_coeff() function
# fieldData = inv_fieldData table from NEON data product "DP1.20120.001"

MN.lw.x = MN.lw %>%
  filter(!str_detect(sampleCondition,
                     "measurement"))
fieldData = macro$inv_fieldData

# simplify fieldData to three columns
field = fieldData %>%
  select(siteID, sampleID, benthicArea) %>%
  distinct()
# add benthicArea column from fieldData to x. This is necessary to calculate number per m2 below

# join by siteID and sampleID
MN.no.damage.taxa <- left_join(MN.lw.x, field, by = c("siteID", "sampleID")) %>% 
  # filter(invertebrateLifeStage %in% c("larva", "pupa")) %>%
  filter(sizeClass >= 3) %>%
  mutate(dw = case_when(formula_type == 1 ~ a * sizeClass^b,
                        formula_type == 2 ~ exp(a + b * log(sizeClass)))) 

saveRDS(MN.no.damage.taxa, file = "data/derived_data/MN.no.damage.taxa.rds")

MN.no.damage <- MN.no.damage.taxa %>% 
  mutate(date = ymd(as.Date(collectDate))) %>% 
  group_by(dw, siteID, date) %>% 
  summarize(n = sum(estimatedTotalCount),
            benthicArea = sum(benthicArea),
            no_m2 = n/benthicArea)

# clean and save ---------------------------------
# filter out NA values in dw
MN.no.damage.nona <- MN.no.damage %>%
  filter(!is.na(dw), !is.na(no_m2)) %>% 
  as_tibble()