# Sky Kunkel #
# Mending the Broken #
# Plot Data #
# 3/15/2024 #

## load libraries ##
library(tidyverse); library(sf)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder
rm(list = ls())

# read data #




sf_use_s2(FALSE)
d <- st_join(kenya_3, afro_6)

df = d %>%
  select(c(q52a,shapeName)) %>%
  filter(c(q52a == 1 | q52a == 2 | q52a == 3 | q52a == 4 | q52a == 5)) %>%
  group_by(shapeName) %>%
  summarize(trust = mean(q52a))


ggplot() + 
  geom_sf(aes(fill = df$trust, geometry = df$geometry)) +
  geom_sf(data = kenya, fill = NA, color = "black") +  # Add the "kenya" layer
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(1.2,3.0)) +
  theme(legend.background = element_rect(color = "black"),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(5,5,5,5)), 
        legend.key.size = unit(0.5, 'cm'))
plot(d)