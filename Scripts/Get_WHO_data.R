##############################################
# Aim: Retrieve cause of death data from WHO
# Date: 17/02/2023
# Notes: Requires Internet connection
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
##############################################

# Download

if(!file.exists("Data/Morticd9")){
  downloader::download("https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd09.zip?sfvrsn=cfa97ceb_3&ua=1", 
                       dest="Data/who.zip", mode="wb") 
  unzip("Data/who.zip", exdir = "Data")
  file.remove("Data/who.zip")
}

if(!file.exists("Data/Morticd10_part1")){
  downloader::download("https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part1.zip?sfvrsn=e2a4f93a_15&ua=1", 
                       dest="Data/who.zip", mode="wb") 
  unzip("Data/who.zip", exdir = "Data")
  file.remove("Data/who.zip")
}

if(!file.exists("Data/Morticd10_part2")){
  downloader::download("https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part2.zip?sfvrsn=6e55000b_3&ua=1", 
                       dest="Data/who.zip", mode="wb") 
  unzip("Data/who.zip", exdir = "Data")
  file.remove("Data/who.zip")
}

if(!file.exists("Data/Morticd10_part3")){
  downloader::download("https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part3.zip?sfvrsn=9f1111a2_7&ua=1", 
                       dest="Data/who.zip", mode="wb") 
  unzip("Data/who.zip", exdir = "Data")
  file.remove("Data/who.zip")
}

if(!file.exists("Data/Morticd10_part4")){
  downloader::download("https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part4.zip?sfvrsn=259c5c23_20&ua=1", 
                       dest="Data/who.zip", mode="wb") 
  unzip("Data/who.zip", exdir = "Data")
  file.remove("Data/who.zip")
}

if(!file.exists("Data/Morticd10_part5")){
  downloader::download("https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part5.zip?sfvrsn=ad970d0b_26&ua=1", 
                       dest="Data/who.zip", mode="wb") 
  unzip("Data/who.zip", exdir = "Data")
  file.remove("Data/who.zip")
}

# Read and merge
# ICD-9
who9 <- read.csv("Data/Morticd9")
# Exclude unclassifiable causes
who9 <- who9 %>% 
  filter(!Cause%in%c("415+","416","420+"))

#ICD-10
who1 <- read.csv("Data/Morticd10_part1")

who2 <- read.csv("Data/Morticd10_part2")

who3 <- read.csv("Data/Morticd10_part3")

who4 <- read.csv("Data/Morticd10_part4")

who5 <- read.csv("Data/Morticd10_part5")

who10 <- cbind(who="10",rbind(who1,who2,who3,who4,who5))

# ICD-9 + ICD-10
who <- rbind(cbind(who="9",who9),who10)
who <- clean_names(who)

save(who, file="Data/who.RData")

# Clean up
rm(list = ls()) 
