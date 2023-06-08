###################################################################
# Aim: Retrieve period lifetables for specific countries from HMD
# Date: 16/02/2023
# Notes: Requires Internet connection, HMD username and password
# Project: Cause-of-death determinants of lifespan inequality
###################################################################

# get needed countries in HMD
XYZ <- c("AUT", "BEL", "BGR", "ESP", "FRATNP", "HUN", "ITA", "NLD", "POL", "PRT")

# get the selected lifetables from HMD
hmd <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))


# convert to data.table
hmd <- data.table(hmd)

# save the data
save(hmd,file="Data/hmd.RData")

# Clean up
rm(list = ls())
