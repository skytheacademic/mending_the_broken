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
kenya_3 = st_read(dsn = "./data/kenya/adm3", 
                layer = "geoBoundaries-KEN-ADM3", 
                stringsAsFactors = F) %>%
  select(c(shapeName, geometry))
proj_crs <- st_crs(kenya)

### read data ###

# Q43A = How much do you trust each of the following, or haven’t you heard enough 
        # about them to say: The President?

# Q27C-KEN = With regard to the last national elections in December 2002: Did you vote?
afro_2 = read_excel("data/afrobarometer/KEN_r2.csv.xlsx") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs) %>%
  mutate(dateintr = ymd(dateintr))
afro_3 = read_excel("data/afrobarometer/KEN_r3.csv.xlsx") %>% # pre treatment data
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs) %>%
  mutate(dateintr = ymd(dateintr))
afro_4 = read_excel("data/afrobarometer/KEN_r4.csv.xlsx") %>% # pre treatment data
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs) %>%
  mutate(dateintr = ymd(dateintr))
afro_5 = read_excel("data/afrobarometer/KEN_r5.csv.xlsx") %>% # during treatment data
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs) %>%
  mutate(dateintr = ymd(dateintr))
afro_6 = read_excel("data/afrobarometer/KEN_r6.csv.xlsx") %>% # post treatment data
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs) %>%
  mutate(dateintr = ymd(dateintr))


sort(Reduce(union, list(
  table(afro_2$location_type_code),
  table(afro_3$location_type_code), 
  table(afro_4$location_type_code), 
  table(afro_5$location_type_code), 
  table(afro_6$location_type_code)
)
))

# Combine all location_type_code values into one data frame
combined_data <- bind_rows(
  afro_2 %>% select(location_type_code) %>% mutate(source = '2'),
  afro_3 %>% select(location_type_code) %>% mutate(source = '3'),
  afro_4 %>% select(location_type_code) %>% mutate(source = '4'),
  afro_5 %>% select(location_type_code) %>% mutate(source = '5'),
  afro_6 %>% select(location_type_code) %>% mutate(source = '6')
) %>%
  as.data.frame() %>%
  select(-c(geometry))

# Create a table of counts for each location_type_code
sort(table(combined_data$location_type_code))


# let's see what levels of aggregation the data are at

# using https://www.geonames.org/export/codes.html, check the location_type_code for each AB dataset

# List of data frames
data_frames <- list(afro_2, afro_3, afro_4, afro_5, afro_6)

# Create a list of tables, one for each location_type_code in each data frame
tables_list <- lapply(data_frames, function(df) {
  table(df$location_type_code)
})

# Print the tables for each data frame
for (i in seq_along(data_frames)) {
  cat("Table for afro_", i+2, ":\n")
  print(tables_list[[i]])
  cat("\n")
}

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
