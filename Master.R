##############################################
# Aim: Master script
# Date: 17/02/2023
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
##############################################

# NB: you need to insert your HMD username and password on lines 45 and 47

# Set-up ------------------------------------------------------------------

# Clear workspace
rm(list = ls()) 

# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get necessary packages
pacman::p_load(HMDHFDplus,   # deal with HMD data
               downloader,   # download data (useful for zip files)
               data.table,   # useful data type
               tidyverse,    # useful set of commands
               janitor,      # data cleaner
               beepr,        # sound effects
               stringr,      # manipulate strings
               ungroup,      # extrapolate grouped ages
               viridis,      # nice palettes
               ggthemes,     # nice ggplot2 modifications
               ggpubr        # package to set up background grids
)

# Load tailor-made functions
source("Scripts/Functions.R")


# Get the data -----------------------------------------------------------

# Download WHO data - online
source("Scripts/Get_WHO_data.R")
# Read saved WHO data - offline. If using, please comment out the previous line.
# load("Data/who.RData")

# Get HMD data - online
# set your username for HMD
us <- ""
# set your password for HMD
pw <- ""
source("Scripts/Get_HMD_data.R")
# Read saved HMD data - offline. If using, please comment out the previous line.
# load("Data/hmd.RData")



# Create multiple decrement lifetables ------------------------------------

source("Scripts/Multiple_decrement_lifetables.R")

# IPM decomposition -------------------------------------------------------

# Select the years
year_first <- 1985
year_middle <- 2000
year_last <- 2015

# Variance
source("Scripts/Variance_decomposition.R")

# Coefficient of variation squared
source("Scripts/CV2_decomposition.R")


# Plots -------------------------------------------------------------------

# Variance
source("Scripts/Variance_plots.R")

# Coefficient of variation squared
source("Scripts/CV2_plots.R")

# Other plots
source("Scripts/General_plots.R")
