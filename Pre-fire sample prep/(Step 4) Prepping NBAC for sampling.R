#Start by filling holes in the polygons. If this step is done after masking to study area or bounding box, some significant holes on the border will be missed

# sf package for GIS operations
library(sf)
# sfheaders package for the remove holes functions
library(sfheaders)

#The AB project had the newest version of NBAC at the time I was redoing NWT. Since we want the two projects to match, I'm juts bringing it in here
NBAC <- st_read("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Data/Original Data/Fires/Polygons/NBAC/NBAC_1972to2024_20250506.shp")

#Filter by year so you're not filling holes in the entire dataset
NBAC_year <- NBAC[NBAC$YEAR >= 2002, ]

NBAC_wo_holes <- sf_remove_holes(NBAC_year) #Fill in holes in each

#This will produce some polygons that are invalid and will throw an error at the later intersect step. This code checks to see how many are invalid and then fixes them
invalid <- st_is_valid(NBAC_wo_holes)
table(invalid)  #FALSE tells you how many are invalid
# Make all polygons valid
NBAC_fixed <- st_make_valid(NBAC_wo_holes)
invalid2 <- st_is_valid(NBAC_fixed)
table(invalid2) #All of them should now be TRUE


file.name = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Processing Data/Fires/NBAC/Step1_Holes_Filled.shp"
st_write(NBAC_fixed, file.name, layer_options = 'SHPT=POLYGON')



#####################################
##########CHANGE PROJECTION##########
#####################################
library(terra)
library(dplyr)

#Load the study area template
ROI <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam.shp")

#Load NBAC
NBAC <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Processing Data/Fires/NBAC/Step1_Holes_Filled.shp")


#Are projections the same?
crs(ROI) == crs(NBAC) #No, change proj of NBAC to match the ROI base layer

NBAC_proj <- project(NBAC, crs(ROI))
#Check again
crs(ROI) == crs(NBAC_proj) #Now they match
gc()
nrow(NBAC_proj) #31,620 fires before masking to study area


##NOTE! From here on is a little different from AB. This time, I filter by year/month and then aggregate before clipping to study area.
##This was done to avoid losing any bits of fires by accident. I did check AB and it didn't seem to be a problem there, but could have been here

########################################################
#####################FILTER BY MONTH####################
########################################################

# 1. Turn the layer into a dataframe
attrs <- as.data.frame(NBAC_proj)
# Attach a guaranteed row ID BEFORE doing anything else
attrs$id <- seq_len(nrow(attrs))

# 2. Convert both date fields to Date class
attrs$HS_SDATE <- as.Date(attrs$HS_SDATE)
attrs$AG_SDATE  <- as.Date(attrs$AG_SDATE)

# 3. Extract month from HS_SDATE (as numeric), keeping NAs
attrs$month_num <- as.numeric(format(attrs$HS_SDATE, "%m"))

# 4. Replace NA months using AG_SDATE
attrs$month_num <- coalesce(
  as.numeric(format(attrs$HS_SDATE, "%m")),
  as.numeric(format(attrs$AG_SDATE, "%m"))
)

# 5. Remove unwanted months
months_to_remove <- c(1, 2, 3, 11, 12)
attrs_filtered <- attrs[!attrs$month_num %in% months_to_remove, ]
#Double check that those with NA for month are like that because neither date field has data
check <- attrs_filtered[is.na(attrs_filtered$month_num), ] #Isolate all the NA ones first
all(is.na(check$HS_SDATE)) #A result of TRUE means all the rows have NA values in the column being checked - this is what we'd expect
all(is.na(check$AG_SDATE)) #In both cases, there are no dates in either field, so it's ok to remove all the polygons with NA in month_num

# 6. Remove rows where month_num is NA
attrs_filtered <- attrs_filtered[!is.na(attrs_filtered$month_num), ]

# 7. Filter the SpatVector using the stable `id` column
Dates_filtered <- NBAC_proj[attrs_filtered$id, ]

# 8. Reassign the filtered attributes back to the SpatVector
# Remove 'id' from the attribute table before assigning back
attrs_filtered$id <- NULL
values(Dates_filtered) <- attrs_filtered
nrow(Dates_filtered) #29404 fires after filtering by dates

#Export to read in later
writeVector(Dates_filtered, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Processing Data/Fires/NBAC/Step2_Dates_Filtered.shp",
            overwrite=T)



###############################################################
###################JOINING DUPLICATE POLYGONS##################
###############################################################

#Clear the environment and load up the shapefile from last time
NBAC <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Processing Data/Fires/NBAC/Step2_Dates_Filtered.shp")
values(NBAC)$month_num <- as.character(values(NBAC)$month_num) #Make month_num a character so it'll be retained at the end
values(NBAC)$YEAR <- as.character(values(NBAC)$YEAR) #Make year a character so it'll be retained at the end
values(NBAC)$NFIREID <- as.character(values(NBAC)$NFIREID) #Make NFIREID a character so it'll be retained at the end


#How many polygons in this filtered version?
nrow(NBAC) #29404
#How many have unique names?
length(unique(values(NBAC)$GID)) #29184
#So some of them are duplicates, as expected
#Check out which ones have duplicates so you can find them in Arc later
duplicated_GIDs <- values(NBAC)$GID[duplicated(values(NBAC)$GID)]
unique(duplicated_GIDs)

# -----------------------------
# 1. Aggregate numeric columns (sum POLY_HA)
# -----------------------------
NBAC_ag <- terra::aggregate(NBAC[, c("GID", "ADJ_HA")], #Chose ADJ_HA over POLY_HA at Rob's suggestion. I believe poly can slightly overestimate fire sizes
                            by = "GID", 
                            dissolve = TRUE, 
                            fun = sum,       
                            na.rm = TRUE)
gc()

# -----------------------------
# 2. Restore non-numeric columns
# -----------------------------
# Convert original attributes to dataframe
attrs <- as.data.frame(NBAC)

# Identify non-numeric columns (excluding GID)
non_num_cols <- names(attrs)[!sapply(attrs, is.numeric) & names(attrs) != "GID"]

# Take first value per GID
agg_non_num <- aggregate(attrs[, non_num_cols], by = list(GID = attrs$GID), FUN = function(x) x[1])

# Merge back to aggregated SpatVector
NBAC_ag <- merge(NBAC_ag, agg_non_num, by = "GID")
nrow(NBAC_ag) #29184
length(unique(values(NBAC_ag)$GID)) #29184
#There are now 29184 fires remaining (same number identified as having unique IDs)



# -----------------------------
# 4. Result: NBAC_ag
# -----------------------------
# - Each GID has exactly one polygon
# - POLY_HA is summed
# - Non-numeric columns are restored


#Quick check here to make sure that any aggregated polys add up properly (they do)
Check <- as.data.frame(NBAC_ag)
Check <- Check %>%      
  filter(GID == "2002_566") 
Orig <- as.data.frame(NBAC)
Orig <- Orig %>%      
  filter(GID == "2002_566") #Works

#Finally, sort so that only fires >= 500ha are kept (to match Quinn's data) and remove prescribed burns. Also get rid of the buffered points, which are just circles
NBAC_out <- NBAC_ag[NBAC_ag$agg_ADJ_HA >= 500, ]
NBAC_out <- NBAC_out[is.na(NBAC_out$PRESCRIBED), ]
NBAC_out <- NBAC_out[!NBAC_out$FIREMAPM == "Buffered points", ] #14 were dropped for this. 

nrow(NBAC_out)
#The final filtered version contains 5887 fires (LOTS were dropped for being under 500ha)


#Export and check out in Arc. Use the list of duplicates from earlier to find those ones
writeVector(NBAC_out, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Processing Data/Fires/NBAC/Step3_Aggregated_500ha.shp",
            overwrite =T)



######################################
##########MASK TO STUDY AREA##########
######################################

#Load the study area template
ROI <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam.shp")

#Load NBAC
NBAC <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Processing Data/Fires/NBAC/Step3_Aggregated_500ha.shp")


#Are projections the same after all the other steps?
crs(ROI) == crs(NBAC) #Yes


#Keep only polygons that fall within the ROI study area
NBAC_SA <- terra::intersect(NBAC, ROI)
gc()
nrow(NBAC_SA) #2523 fires after masking to study area

#Export the final layer
writeVector(NBAC_SA, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Fires/Polygons/Final_NBAC.shp")

