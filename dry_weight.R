#Dry weight and mat.c 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(neonUtilities)
library(neonOS)
library(terra)
library(tidymodels)

#Main data object
bugData


#formulas for dry weights
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

#Listing families
DW_fam <- sort(unique(Bug_DATA$family))
#Function to graph each family on a graph, and by there dry weight average and by the mat.c 
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
#print(invrt_dw_list)


n_sites_av <- Bug_DATA |>
  filter(family %in% pres_bugs2_v)















