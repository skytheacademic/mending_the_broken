# Sky Kunkel #
# Mending the Broken #
# Clean Data #
# 1/29/2024 #

# load libraries
library(tidyverse); library(janitor); library(readxl)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

### read data ###
rm(list = ls())
afro_2 = read_excel("data/afrobarometer/KEN_r2.csv.xlsx")
afro_3 = read_excel("data/afrobarometer/KEN_r3.csv.xlsx")
afro_4 = read_excel("data/afrobarometer/KEN_r4.csv.xlsx")
afro_5 = read_excel("data/afrobarometer/KEN_r5.csv.xlsx")
afro_6 = read_excel("data/afrobarometer/KEN_r6.csv.xlsx")


# let's see what levels of aggregation the data are at
