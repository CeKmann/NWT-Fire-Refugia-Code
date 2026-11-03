library(data.table)
library(terra)


##Read in all annual files and bind them together into a single dataframe
data_dir <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6"

files <- list.files(
  path = data_dir,
  pattern = "^Initial_Sample_[0-9]{4}_V6.csv$",
  full.names = TRUE
)

all_samples <- rbindlist(
  lapply(files, fread),
  use.names = TRUE,
  fill = FALSE
)
class(all_samples) #It's a data.table/data.frame

all_samples <- as.data.frame(all_samples)
class(all_samples)
fires <- unique(all_samples$GID) #2523 fires, correct number




#######Extract FRUs#####
FRU <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/FRU/NWT_FRU_30m.tif")
FRU

all_samples$FRU <- extract(FRU, all_samples[, c("x", "y")], ID = FALSE)[, 1]
gc()




######Assign unique IDs to all points########
all_samples$Unique_ID <- seq_len(nrow(all_samples))



#######Convert the coordinates to WGS#######
names(all_samples)[names(all_samples) == "x"] <- "Lam_x"
names(all_samples)[names(all_samples) == "y"] <- "Lam_y"
colSums(is.na(all_samples)) #Lots of NAs but all in columns where they're fine (nothing in coords columns)

# Create a SpatVector from the coordinate columns (EPSG:3979)
pts_3979 <- vect(
  all_samples,
  geom = c("Lam_x", "Lam_y"),
  crs  = "EPSG:3979",
  keepgeom = TRUE
)

# Reproject to WGS84
pts_wgs <- project(pts_3979, "EPSG:4326")

# Extract WGS coordinates
coords_wgs <- crds(pts_wgs)

# Add to data.frame with explicit column names for GEE
all_samples$longitude <- coords_wgs[, 1]
all_samples$latitude  <- coords_wgs[, 2]



######Retain a max of 50k points for each FRU, except for validation years (2022-2024), which are retained in full######
library(dplyr)

all_samples$FRU <- as.factor(all_samples$FRU)

#Break off 2022-2024, since these will be for validation and not truncated at this stage
Val <- all_samples %>%
  filter(YEAR %in% 2022:2024)
Training <- all_samples %>%
  filter(!YEAR %in% 2022:2024)


FRU_List <- as.list(levels(Training$FRU))

set.seed(123)  # for reproducibility

#fru_val=20
FRU_Out <- lapply(FRU_List, function(fru_val) {
  Current_FRU <- Training %>% filter(FRU == fru_val)
  
  if (nrow(Current_FRU) > 50000) {
    Current_FRU[sample(nrow(Current_FRU), 50000), ]
  } else {
    Current_FRU
  }
}) %>%
  bind_rows()

#Check to see how many samples are now in each FRU and compare that to how many were there originally. The only ones that should have changed are those >50k originally
original_counts <- Training %>%
  group_by(FRU) %>%
  summarise(original_n = n())

sampled_counts <- FRU_Out %>%
  group_by(FRU) %>%
  summarise(sampled_n = n())

compare_counts <- original_counts %>%
  full_join(sampled_counts, by = "FRU") %>%
  arrange(FRU)

compare_counts #Results show code is working as intended
#Note: there were 47 points that had NAs for the FRU value (probably edges or small gaps in the FRU layer. The 50k sample step removes these, which is fine)


#Get NAs out of FRU column for the Val sample
Val_clean <- Val %>%
  filter(!is.na(FRU)) #There were 14 points removed

#Add those validation samples back to the main one and export
Done <- rbind(Val_clean, FRU_Out) #1,817,169 in the final dataset



######Export######
write.csv(Done, file = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 2) Prepping sample for GEE/A_NWT_FRU_WGS_50k_V1.csv")



##Didn't do the following for NWT, since it worked out fine for AB, but keeping the code in case it's needed later
#######Just check and make sure those NAs are legit#######
#I checked and they are. All come from very edges of study area, where's there's 1 cell (or less) mismatch between the base/fru rasters and the base vect used for intersecting fire polys (and therefore fire rasters during sampling)
#I dug in with ChatGPT and it says this is normal and to be expected - all that's needed is to drop those NAs (which was done above)
NA_FRUs <- Training %>%
  filter(is.na(FRU))
template <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Data/Final Data/Study Area/Base_30m_Lam.tif")
template_crs <- st_crs(crs(template))


library(sf)
NA_FRUs_sf <- st_as_sf(NA_FRUs, coords = c("Lam_x", "Lam_y"), crs = template_crs)  

# Export as shapefile
st_write(NA_FRUs_sf, "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Sample/Prepping sample for GEE (Step 2)/Checking points/FRU_NAs.shp")
