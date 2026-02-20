# tidy modelling start

# this is based on the tutorial here: https://rpubs.com/DanielZhang/1179326

# install.packages("palmerpenguins")

library(tidyverse)
library(palmerpenguins)
library(broom) # install?

penguins
# "species" and "islands" are like taxa and site
# bill length and body mass are like mat.c and dw

penguins |>
  group_by(island, species) |>
  nest() # makes a list-column

# figure out one lm, then make it a function
mod1 <- lm(bill_length_mm ~ body_mass_g, data = penguins)
summary(mod1)

# fit an lm for each "group" where y = bill_length ~ x = body_mass
# dw ~ mat.c or latitude
penguins |>
  group_by(island, species) |>
  nest() |>
  mutate(lm_fits = map(
    .x = data,
    .f = ~lm(data = .x, 
             formula = bill_length_mm ~ body_mass_g)
  )) |>
  mutate(tidy_fits = map(
    lm_fits, tidy
  )) |>
  unnest(tidy_fits) |>
  filter(
    term != "(Intercept)",
    p.value < 0.001)





