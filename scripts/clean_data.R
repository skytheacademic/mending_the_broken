# Sky Kunkel #
# Mending the Broken #
# Clean Data #
# 1/29/2024 #

# load libraries
library(tidyverse); library(janitor); library(readxl); library(sf)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

### read Kenya map data ###
rm(list = ls())
kenya = st_read(dsn = "./data/kenya/adm2", 
                layer = "geoBoundaries-KEN-ADM2", 
                stringsAsFactors = F) %>%
  select(c(shapeName, geometry))
proj_crs <- st_crs(kenya)

### read data ###
afro_2 = read_excel("data/afrobarometer/KEN_r2.csv.xlsx") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs)
afro_2 <- st_as_sf(afro_mali, coords = c("longitude", "latitude"), crs = proj_crs)
# Q43A = How much do you trust each of the following, or haven’t you heard enough 
        # about them to say: The President?

# Q27C-KEN = With regard to the last national elections in December 2002: Did you vote?

afro_3 = read_excel("data/afrobarometer/KEN_r3.csv.xlsx")
afro_4 = read_excel("data/afrobarometer/KEN_r4.csv.xlsx")
afro_5 = read_excel("data/afrobarometer/KEN_r5.csv.xlsx")
afro_6 = read_excel("data/afrobarometer/KEN_r6.csv.xlsx")


# let's see what levels of aggregation the data are at

sf_use_s2(FALSE)
d <- st_join(kenya, afro_2)

df = d %>%
  select(c(q43a,shapeName)) %>%
  filter(c(q43a == 1 | q43a == 2 | q43a == 3 | q43a == 4 | q43a == 5)) %>%
  group_by(shapeName) %>%
  summarize(trust = mean(q43a))


ggplot() + 
  geom_sf(aes(fill = df$trust, geometry = df$geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(1.2,3.0)) +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.25, 0.65),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(5,5,5,5)), 
        legend.key.size = unit(0.5, 'cm'))
plot(d)
