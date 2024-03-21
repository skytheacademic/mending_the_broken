# Sky Kunkel #
# Mending the Broken #
# Clean Data #
# 1/29/2024 #

# load libraries
library(tidyverse); library(janitor); library(readxl); library(sf); library(haven)

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
proj_crs = st_crs(kenya)

### read data ###
afro_2 = read_excel("data/afrobarometer/KEN_r2.csv.xlsx") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = proj_crs) %>% # read it in as map data
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
afro_7 = read_sav("data/afrobarometer/Kenya_R7.Data_7Feb19_release.w.local.info.sav")
  # post treatment data
  # this round is weird for coordinates, so I need to modify them before turning into map data
names(afro_7) = tolower(names(afro_7))


afro_7 = afro_7 %>%
  # Ensure ea_gps_other is character for regex operations
  mutate(ea_gps_other = as.character(ea_gps_other)) %>%
  # Extract and convert latitude and longitude from ea_gps_other
  rowwise() %>%
  mutate(
    # Latitude extraction and sign adjustment
    la_extracted = if_else(!is.na(ea_gps_other), as.numeric(str_extract(ea_gps_other, "\\d+\\.\\d+(?=°)")), NA_real_),
    la_sign = if_else(str_detect(ea_gps_other, "^S"), -1, 1),
    # Longitude extraction
    lo_extracted = as.numeric(str_extract(ea_gps_other, "(?<=E )\\d+\\.\\d+")),
    # For longitude, since "E" indicates east which should be positive, we directly assign it without sign adjustment
    lo_sign = 1 # This ensures all "E" values are treated as positive
  ) %>%
  ungroup() %>%
  # Apply extracted values with sign adjustment for latitude and direct assignment for longitude
  mutate(
    ea_gps_la = if_else(is.na(ea_gps_la), la_extracted * la_sign, ea_gps_la),
    ea_gps_lo = if_else(is.na(ea_gps_lo), lo_extracted * lo_sign, ea_gps_lo)
  ) %>%
  # Remove temporary columns
  select(-c(la_extracted, la_sign, lo_extracted, lo_sign))

afro_7 = afro_7 %>%
  st_as_sf(coords = c("ea_gps_lo", "ea_gps_la"), crs = proj_crs) %>%
  mutate(dateintr = ymd(dateintr)) 






### filter the variables ###

## Round 2 ##
afro_2 = afro_2 %>%
  # first line should be identical across datasets
  select(c(regionstring, locationlevel1, locationlevel2, locationlevel3, locationlevel4, townvill, 
           precision_code, geographic_exactness,
  # following lines may have some overlap but different across rounds
           q96, q80, q84, q96new, urbrur, q89, q27, q43a, q43b, q43j)) %>%
  # mutate vars
  mutate(adm1 = locationlevel1, adm2 = locationlevel2, adm3 = locationlevel3, adm4 = locationlevel4) %>%
  # mutate round specific vars
  mutate(gender = q96, age = q80, edu = q84, race = q96new, urb = urbrur, employed = q89, reg_vote = q27,
         trust_pres = q43a, trust_par = q43b, trust_court = q43j, 
         round = 2) %>% 
  # drop non-mutated variables
  select(-c(locationlevel1, locationlevel2, locationlevel3, locationlevel4, q96, q80, q84, q96new, urbrur, 
            q89, q27, q43a, q43b, q43j)) %>% 
  relocate("geometry", .after = last_col())
# where is q27c? 

## Round 3 ##
afro_3 = afro_3 %>%
  select(c(district, townvill, 
           precision_code, geographic_exactness,
           # following lines may have some overlap but different across rounds
           q101, q1, q90, q102, urbrur, q94, q30, q55a, q55b, q55i)) %>%
  # mutate round specific vars
  mutate(gender = q101, age = q1, edu = q90, race = q102, urb = urbrur, employed = q94, vote = q30,
         trust_pres = q55a, trust_par = q55b, trust_court = q55i,
         round = 3) %>% 
  # drop non-mutated variables
  select(-c(q101, q1, q90, q102, urbrur, 
            q94, q30, q55a, q55b, q55i)) %>% 
  relocate("geometry", .after = last_col())

## Round 4 ##
afro_4 = afro_4 %>%
  select(c(district, townvill, 
           precision_code, geographic_exactness,
           # following lines may have some overlap but different across rounds
           q101, q1, q89, q102, urbrur, q94, q23d, q49a, q49b, q49h)) %>%
  # mutate round specific vars
  mutate(gender = q101, age = q1, edu = q89, race = q102, urb = urbrur, employed = q94, vote = q23d,
         trust_pres = q49a, trust_par = q49b, trust_court = q49h,
         round = 4) %>% 
  # drop non-mutated variables
  select(-c(q101, q1, q89, q102, urbrur, q94, q23d, q49a, q49b, q49h)) %>% 
  relocate("geometry", .after = last_col())

## Round 5 ##
afro_5 = afro_5 %>%
  select(c(district, townvill, locationlevel1, locationlevel2, locationlevel3, locationlevel4,
           precision_code, geographic_exactness,
           # following lines may have some overlap but different across rounds
           q101, q1, q97, q102, urbrur, q96, q27, q59a, q59b, q59j)) %>%
  # mutate vars
  mutate(adm1 = locationlevel1, adm2 = locationlevel2, adm3 = locationlevel3, adm4 = locationlevel4) %>%
  # mutate round specific vars
  mutate(gender = q101, age = q1, edu = q97, race = q102, urb = urbrur, employed = q96, vote = q27,
         trust_pres = q59a, trust_par = q59b, trust_court = q59j, 
         round = 5) %>% 
  # drop non-mutated variables
  select(-c(q101, q1, q97, q102, urbrur, q96, q27, q59a, q59b, q59j)) %>% 
  relocate("geometry", .after = last_col())
# NONE OF THE R5 ONLY VARS ARE PRESENT IN DATA

## Round 6 ##
afro_6 = afro_6 %>%
  select(c(townvill, locationlevel1, locationlevel2, locationlevel3, locationlevel4,
           precision_code, geographic_exactness,
           # following lines may have some overlap but different across rounds
           q101, q1, q97, q102, urbrur, q95, q21, q52a, q52b, q52j)) %>%
  # mutate vars
  mutate(adm1 = locationlevel1, adm2 = locationlevel2, adm3 = locationlevel3, adm4 = locationlevel4) %>%
  # mutate round specific vars
  mutate(gender = q101, age = q1, edu = q97, race = q102, urb = urbrur, employed = q95, vote = q21,
         trust_pres = q52a, trust_par = q52b, trust_court = q52j, 
         round = 6) %>% 
  # drop non-mutated variables
  select(-c(q101, q1, q97, q102, urbrur, q95, q21, q52a, q52b, q52j, 
            locationlevel1, locationlevel2, locationlevel3, locationlevel4)) %>% 
  relocate("geometry", .after = last_col())

## Round 7 ##
afro_7 = afro_7 %>%
  select(c(townvill,
           # following lines may have some overlap but different across rounds
           q101, q1, q97, q102, urbrur, q94, q22, q43a, q43b, q43i)) %>%
  # mutate round specific vars
  mutate(gender = q101, age = q1, edu = q97, race = q102, urb = urbrur, employed = q94, vote = q22,
         trust_pres = q43a, trust_par = q43b, trust_court = q43i) %>% 
  # drop non-mutated variables
  select(-c(q101, q1, q97, q102, urbrur, q94, q22, q43a, q43b, q43i, 
            round = 7)) %>% 
  relocate("geometry", .after = last_col())
# no more precision codes or geographic_exactness since enumerators now use coordinates of device


### some vars change in coding over time ###
  # R2 and R3 have unique voting codes, and R4 onward have identical codes #

# recode voting to dichotomous (o if eligible but didn't vote, 1 if voted or tried to vote)
afro_3$vote_binary = NA
afro_3$vote_binary[afro_3$vote==2] = 0
afro_3$vote_binary[afro_3$vote==1 | afro_3$vote==3 | afro_3$vote==4 | afro_3$vote==5 | afro_3$vote==6] = 1

## code a variable so we can run robustness check later to remove tried to vote but didnt ##

  # Values 0-3 do not change across rounds for trust in parliament, but the refused/don't know answers change
  # Trust in court of law is same as above ^^^^^


# Recoding -1 to NA across all columns (-1 means missing data in AfroB data)
afro_2 = afro_2 %>%
  mutate(across(where(is.numeric), ~na_if(., -1)))
afro_3 = afro_3 %>%
  mutate(across(where(is.numeric), ~na_if(., -1)))
afro_4 = afro_4 %>%
  mutate(across(where(is.numeric), ~na_if(., -1)))
afro_5 = afro_5 %>%
  mutate(across(where(is.numeric), ~na_if(., -1)))
afro_6 = afro_6 %>%
  mutate(across(where(is.numeric), ~na_if(., -1)))
afro_7 = afro_7 %>%
  mutate(across(where(is.numeric), ~na_if(., -1)))