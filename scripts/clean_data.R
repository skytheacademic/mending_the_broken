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
kenya = st_read(dsn = "./data/kenya/adm0", 
                  layer = "geoBoundaries-KEN-ADM0", 
                  stringsAsFactors = F) %>%
  select(c(shapeName, geometry))
kenya_3 = st_read(dsn = "./data/kenya/adm2", 
                layer = "geoBoundaries-KEN-ADM2", 
                stringsAsFactors = F) %>%
  select(c(shapeName, geometry))
proj_crs <- st_crs(kenya)

### read data ###
afro_2 = read_excel("data/afrobarometer/KEN_r2.csv.xlsx") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs)
# Q43A = How much do you trust each of the following, or haven’t you heard enough 
        # about them to say: The President?

# Q27C-KEN = With regard to the last national elections in December 2002: Did you vote?

afro_3 = read_excel("data/afrobarometer/KEN_r3.csv.xlsx") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs) 
afro_4 = read_excel("data/afrobarometer/KEN_r4.csv.xlsx") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs)
afro_5 = read_excel("data/afrobarometer/KEN_r5.csv.xlsx") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs)
afro_6 = read_excel("data/afrobarometer/KEN_r6.csv.xlsx") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs)


sort(Reduce(union, list(
  table(afro_2$location_type_code),
  table(afro_3$location_type_code), 
  table(afro_4$location_type_code), 
  table(afro_5$location_type_code), 
  table(afro_6$location_type_code)
)
))

# Combine all location_type_code values into one data frame
combined_data <- data.frame(
  location_type_code = c(
    afro_2$location_type_code,
    afro_3$location_type_code,
    afro_4$location_type_code,
    afro_5$location_type_code,
    afro_6$location_type_code
  )
)

# Create a table of counts for each location_type_code
sort(table(combined_data$location_type_code))


# let's see what levels of aggregation the data are at

# using https://www.geonames.org/export/codes.html, check the location_type_code for each AB dataset

# R2
  # "ADM1"  "ADM1H" "ADM2"  "ADM2H" "ADM3"  "ADM4"  "ADMD"  "ADMF"  "AREA"  "FCL"   "GULF"  "MKT"   "PPL"   "PPLA"  "PPLX"  "SCH"   "WTRH" 
# R3
  # "ADM1"  "ADM2"  "ADM2H" "ADMD"  "AIRB"  "AREA"  "HLL"   "PPL"   "PPLA"  "PPLX"  "SCH"   "STM"   "UNIV"
# R4
  # "ADM1"  "ADM1H" "ADM2"  "ADM2H" "ADMD"  "AREA"  "CTRM"  "FCL"   "HLLS"  "HSP"   "MKT"   "PPL"   "PPLA"  "PPLX"  "PRK"   "SCH"
# R5
  # "ADM1"  "ADM2"  "ADM2H" "ADM3"  "ADMD"  "AREA"  "HTL"   "ML"    "MT"    "PCLI"  "PPL"   "PPLA"  "PPLA2" "PPLC"  "PPLX"  "SCH"
# R6
  # "ADM1" "ADM2" "ADM3" "ADMD" "AREA" "BDG"  "EST"  "FCL"  "HLL"  "HSP"  "LCTY" "MALL" "MKT"  "PPL"  "PPLA" "PPLX" "SCH"


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
