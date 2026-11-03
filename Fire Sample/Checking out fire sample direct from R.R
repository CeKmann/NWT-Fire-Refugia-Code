library(terra)
library(dplyr)
library(data.table)  # fast reading and binding

#################################################################################################################################################
###########How many fires in the final sample? Should be 2 less than in NBAC, as 2 fires were flagged as not having associated rasters###########
#################################################################################################################################################
NBAC <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Fires/Polygons/Final_NBAC.shp")
NBAC_nfire <- unique(NBAC$GID) #2523 fires in NBAC

# Path to your folder
folder_path <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6"
# List all CSV files in the folder
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# Read all files and store in a list (fast with fread)
data_list <- lapply(files, fread)
# Combine all into one big data.table
big_data <- rbindlist(data_list, use.names = TRUE)
# Optional: convert to data.frame if needed
big_data <- as.data.frame(big_data)

Sample_nfire <- unique(big_data$GID) #2523. Correct number of fires
#################################################################################################################################################



##############################################################################
###########Have any points been counted twice within the same year?###########
##############################################################################
dt <- as.data.table(big_data)

# This will give you rows that are duplicated coordinates within the same year
duplicates_within_year <- dt[ , .N, by = .(YEAR, x, y)][N > 1]
duplicates_within_year #There are no duplicated coordinates within the same years

#This one checks for duplicate coordinates between all the years
duplicates_across_years <- dt[ , .N, by = .(x, y)][N > 1]
duplicates_across_years #There are 3032 duplicated coordinates across all the years (reburns)

#Which fires/years do these duplicates come from?
# Join back to the original table to see which fires (GID) they belong to
duplicates_info <- dt[duplicates_across_years, on = .(x, y)]
# Optional: view relevant columns only
duplicates_info[, .(year, GID, x, y)] #A lot of these seem to be recent reburns of fires from 2002. I think this should be ok
##############################################################################



########################################################################################################################
######################Break off some samples and check to make sure they match their inputs in Arc######################
########################################################################################################################
library(sf)

template <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_30m.tif")
template_crs <- st_crs(crs(template))

Year1 <- read.csv("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6/Initial_Sample_2004_V6.csv")
Year2 <- read.csv("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6/Initial_Sample_2007_V6.csv")
Year3 <- read.csv("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6/Initial_Sample_2023_V6.csv")

Y1_Checks <- Year1 %>%
  filter(GID == "2004_308" | GID == "2004_217")
Y2_Checks <- Year2 %>%
  filter(GID == "2007_201" | GID == "2007_154")
Y3_Checks <- Year3 %>%
  filter(GID == "2023_24" | GID == "2023_83")


# Convert each sample to an sf object and give crs
Y1_sf <- st_as_sf(Y1_Checks, coords = c("x", "y"), crs = template_crs)  
Y2_sf <- st_as_sf(Y2_Checks, coords = c("x", "y"), crs = template_crs)
Y3_sf <- st_as_sf(Y3_Checks, coords = c("x", "y"), crs = template_crs)

# Check geometries
all(st_is_valid(Y1_sf))  # should return TRUE (all of them do)
all(st_is_valid(Y2_sf))
all(st_is_valid(Y3_sf))

st_crs(Y1_sf) == st_crs(template_crs)

#Write to disc
st_write(Y1_sf, "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6/Checking in Arc/Year_2004_Checks.shp")
st_write(Y2_sf, "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6/Checking in Arc/Year_2007_Checks.shp")
st_write(Y3_sf, "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6/Checking in Arc/Year_2023_Checks.shp")


