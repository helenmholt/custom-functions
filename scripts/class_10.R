#10/23/2025
#Class 10 

#load libraries
library(tidyverse)
library(rvc)
library(patchwork)

#get data 
USVI <- getRvcData(years = 2017:2023, regions = c("STTSTJ", "STX"))

#explore data
#glimpse(USVI)

#what do we need to do? 
#need dataframe and species in function
#create 3 plots, with density, occurance, and biomass 

RH_density <- getDomainDensity(x = USVI, species = "EPI GUTT") %>%
  mutate(se = sqrt(var))

RH_occ <- getDomainOccurrence(x = USVI, species = "EPI GUTT") %>%
  mutate(se = sqrt(var))

RH_biomass <- getDomainBiomass(x = USVI, species = "EPI GUTT") %>%
  mutate(se = sqrt(var))


density_plot <- ggplot (RH_density, aes(x = YEAR, y = density, group = REGION)) + 
  geom_line(aes(color = REGION)) + geom_point(aes(color = REGION)) + geom_errorbar(aes(ymin = density - se, ymax = density + se, color = REGION))

print(density_plot)

occ_plot <- ggplot (RH_occ, aes(x = YEAR, y = occurrence, group = REGION)) + 
  geom_line(aes(color = REGION)) + geom_point(aes(color = REGION)) + geom_errorbar(aes(ymin = occurrence - se, ymax = occurrence + se, color = REGION))

print(occ_plot)

bio_plot <- ggplot (RH_biomass, aes(x = YEAR, y = biomass, group = REGION)) + 
  geom_line(aes(color = REGION)) + geom_point(aes(color = REGION)) + geom_errorbar(aes(ymin = biomass - se, ymax = biomass + se, color = REGION))

print(bio_plot)

#patchwork 
patchwork <- (density_plot / occ_plot / bio_plot) + plot_layout(guides = "collect")

print (patchwork)

#function
#NEED {{}} for ggplot aes x, y, Z, ex: aes({{x = blah}}, {{y = blah}})

fish_graph_funct <- function(df, species) {
  
  density <- getDomainDensity(x = df, species = species) %>%
    mutate(se = sqrt(var))
  
  occ <- getDomainOccurrence(x = df, species = species) %>%
    mutate(se = sqrt(var))
  
  biomass <- getDomainBiomass(x = USVI, species = species) %>%
    mutate(se = sqrt(var))
  
  density_plot <- ggplot (density, aes(x = YEAR, y = density, group = REGION)) + 
    geom_line(aes(color = REGION)) + 
    geom_point(aes(color = REGION)) + 
    geom_errorbar(aes(ymin = density - se, 
                      ymax = density + se, color = REGION))

  
  occ_plot <- ggplot (occ, aes(x = YEAR, y = occurrence, group = REGION)) + 
    geom_line(aes(color = REGION)) + 
    geom_point(aes(color = REGION)) + 
    geom_errorbar(aes(ymin = occurrence - se, 
                      ymax = occurrence + se, color = REGION))

  
  bio_plot <- ggplot (biomass, aes(x = YEAR, y = biomass, group = REGION)) + 
    geom_line(aes(color = REGION)) + 
    geom_point(aes(color = REGION)) + 
    geom_errorbar(aes(ymin = biomass - se, 
                      ymax = biomass + se, color = REGION))
  
  
  patchwork <- (density_plot / occ_plot / bio_plot) + 
    plot_layout(guides = "collect") + 
    plot_annotation(species, theme = theme(plot.title = 
                                             element_text(hjust = 0.5)))
  
  return(patchwork)
}

fish_graph_funct(USVI, "EPI GUTT")
