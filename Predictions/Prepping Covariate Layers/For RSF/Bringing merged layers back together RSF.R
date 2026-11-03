library(terra)
library(dplyr)
library(tibble)
library(data.table)

#Load dataframe
df <- fread("Z:/Christine/NWT Fire Refugia Project Z/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)

df$FRU <- as.factor(df$FRU)

# FRU_26 <- df %>%
#   filter(FRU == 26 & (ecozone == 5 | ecozone == 6)) %>%
#   droplevels()

FRU_24 <- df %>%
  filter(FRU == 24) %>%
  droplevels()

#Isolate each FRU and pull out the variables which need to be averaged (SCANFI, fires, open areas, cutblocks, Hermo non-fuels)
Needed <- FRU_24 %>%
  select(
    #Disturbance
    "Cutblock_0_10_1200m", "Cutblock_0_10_300m", "Cutblock_10_20_1200m", "Cutblock_10_20_300m", 
    "Fire_10_20_1200m", "Fire_10_20_300m", "Fire_1_10_1200m",  "Fire_1_10_300m", "Fire_20_30_1200m", "Fire_20_30_300m", 
    "Insect_0_10_1200m", "Insect_0_10_300m", "Insect_10_20_1200m", "Insect_10_20_300m",       
    #SCANFI/Open areas fuels (these versions of soil, etc, are taken as 'fuels' and averaged). Herm barren, rock, snow/ice are taken as-is like terrain            
    "SCANFI_Biomass_1200m", "SCANFI_Biomass_300m", "SCANFI_Closure_1200m", "SCANFI_Closure_300m", 
    "SCANFI_Decid_1200m", "SCANFI_Decid_300m", "SCANFI_Height_1200m", "SCANFI_Height_300m",     
    "SCANFI_Other_Con_1200m", "SCANFI_Other_Con_300m", "SCANFI_Pine_1200m", "SCANFI_Pine_300m",        
    "SCANFI_Herbs_1200m", "SCANFI_Herbs_300m", "SCANFI_Lichen_1200m","SCANFI_Lichen_300m", "SCANFI_Rock_1200m", "SCANFI_Rock_300m",
    "SCANFI_Shrub_Tall_1200m", "SCANFI_Shrub_Tall_300m", "SCANFI_Shrub_Low_1200m", "SCANFI_Shrub_Low_300m",
    "SCANFI_Soil_1200m", "SCANFI_Soil_300m")
#Any NAs? (Shouldn't be for these)
if (anyNA(Needed)) {
  print(colSums(is.na(Needed)))
}

#For every column, find the average within the full sample
Means <- colMeans(Needed, na.rm = FALSE) #Already know there are no NAs, but this will catch them if any do happen to occur and aren't noticed earlier
head(Means)
tail(Means)


#Load base layer for predictions and turn all values to 0
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")
Blank <- ROI*0

#Export
for (var in names(Means)) {
  Out <- Blank + Means[var]
  
  writeRaster(
    Out,
    filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Layers for Topo Preds V6/Singles before processing/FRU_24_HALF", var, ".tif"),
    overwrite = TRUE
  )
}




###########################################################
########################For 24/26##########################
###########################################################

######################################################
#####FIRE WEATHER
######################################################
#--------------------------------------------------
# Paths
#--------------------------------------------------
weather_base <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/RSF V6/Fire Weather/For singles before merging"
weather_out <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/RSF V6/Fire Weather/FRU 24 26"

mask_dir <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Individual/Singles before grouping"

dir.create(weather_out, recursive = TRUE, showWarnings = FALSE)

#--------------------------------------------------
# Masks
#--------------------------------------------------

m24 <- rast(file.path(mask_dir, "FRU_24.tif"))
m26 <- rast(file.path(mask_dir, "FRU_26_shield.tif"))

#--------------------------------------------------
# Files
#--------------------------------------------------

files24 <- list.files(
  file.path(weather_base, "FRU 24"),
  pattern = "\\.tif$",
  full.names = TRUE
)

#--------------------------------------------------
# Loop
#--------------------------------------------------

for (f24 in files24) {
  
  cat("\nProcessing fire weather:", basename(f24), "\n")
  
  f26 <- file.path(weather_base, "FRU 26 shield", basename(f24))
  
  if (!file.exists(f26)) {
    cat("Missing FRU 26 file, skipping\n")
    next
  }
  
  # Read rasters
  r24 <- rast(f24)
  r26 <- rast(f26)
  
  # Mask each FRU separately
  r24 <- mask(r24, m24)
  r26 <- mask(r26, m26)
  
  # Merge
  out <- merge(r24, r26)
  
  # Keep original filename
  out_file <- file.path(weather_out, basename(f24))
  
  writeRaster(out, out_file, overwrite = TRUE)
  
  # Cleanup
  rm(r24, r26, out)
  gc()
  
  cat("Finished:", basename(out_file), "\n")
}







###########################################################
########################For 37/38/42#######################
###########################################################

######################################################
#####FIRE WEATHER
######################################################
#--------------------------------------------------
# Paths
#--------------------------------------------------
weather_base <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/RSF V6/Fire Weather/For singles before merging"
weather_out <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/RSF V6/Fire Weather/FRU 37 38 42"

mask_dir <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Individual/Singles before grouping"

dir.create(weather_out, recursive = TRUE, showWarnings = FALSE)

#--------------------------------------------------
# Masks
#--------------------------------------------------

m37 <- rast(file.path(mask_dir, "FRU_37.tif"))
m38 <- rast(file.path(mask_dir, "FRU_38.tif"))
m42 <- rast(file.path(mask_dir, "FRU_42.tif"))

#--------------------------------------------------
# Files
#--------------------------------------------------

files37 <- list.files(
  file.path(weather_base, "FRU 37"),
  pattern = "\\.tif$",
  full.names = TRUE
)

#--------------------------------------------------
# Loop
#--------------------------------------------------

for (f37 in files37) {
  
  cat("\nProcessing fire weather:", basename(f37), "\n")
  
  f38 <- file.path(weather_base, "FRU 38", basename(f37))
  f42 <- file.path(weather_base, "FRU 42", basename(f37))
  
  if (!file.exists(f38)) {
    cat("Missing FRU 38 file, skipping\n")
    next
  }
  
  if (!file.exists(f42)) {
    cat("Missing FRU 42 file, skipping\n")
    next
  }
  
  # Read rasters
  r37 <- rast(f37)
  r38 <- rast(f38)
  r42 <- rast(f42)
  
  # Mask each FRU separately
  r37 <- mask(r37, m37)
  r38 <- mask(r38, m38)
  r42 <- mask(r42, m42)
  
  # Merge all three
  out <- merge(r37, r38, r42)
  
  # Keep original filename
  out_file <- file.path(weather_out, basename(f37))
  
  writeRaster(out, out_file, overwrite = TRUE)
  
  # Cleanup
  rm(r37, r38, r42, out)
  gc()
  
  cat("Finished:", basename(out_file), "\n")
}



######################################################
#####THE REST
######################################################
# Bring all layers back in, clip them to their FRUs, and then merge
mask_dir <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Individual"
out_dir  <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Layers for Topo Preds V6"

# Predictor files
files37 <- list.files(
  "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Layers for Topo Preds V6/Singles before processing",
  pattern = "^FRU_37_.*\\.tif$",
  full.names = TRUE
)

# Read masks once (more efficient)
m37 <- rast(file.path(mask_dir, "FRU_37.tif"))
m38 <- rast(file.path(mask_dir, "FRU_38.tif"))
m42 <- rast(file.path(mask_dir, "FRU_42.tif"))

for(f37 in files37) {
  
  cat("\n============================\n")
  cat("Processing:", basename(f37), "\n")
  
  # Match companion files
  f38 <- gsub("FRU_37", "FRU_38", f37)
  f42 <- gsub("FRU_37", "FRU_42", f37)
  
  cat("Paired with:", basename(f38), "\n")
  cat("Paired with:", basename(f42), "\n")
  
  # Skip if any file is missing
  if(!file.exists(f38)) {
    cat("Missing FRU 38 file, skipping\n")
    next
  }
  
  if(!file.exists(f42)) {
    cat("Missing FRU 42 file, skipping\n")
    next
  }
  
  # Read rasters
  r37 <- rast(f37)
  r38 <- rast(f38)
  r42 <- rast(f42)
  
  # Optional geometry checks
  if(!compareGeom(r37, r38, stopOnError = FALSE))
    cat("WARNING: geometry mismatch between FRU 37 and FRU 38\n")
  
  if(!compareGeom(r37, r42, stopOnError = FALSE))
    cat("WARNING: geometry mismatch between FRU 37 and FRU 42\n")
  
  # Mask rasters
  r37_m <- mask(r37, m37)
  r38_m <- mask(r38, m38)
  r42_m <- mask(r42, m42)
  
  # Merge (non-overlapping FRUs)
  out <- merge(r37_m, r38_m, r42_m)
  
  # -------------------------
  # CLEAN OUTPUT NAMING
  # -------------------------
  
  base <- basename(f37)
  
  # Remove HALF
  base <- sub("HALF", "", base)
  
  # Replace FRU_37 with FRU_37_38_42
  base <- sub("^FRU_37", "FRU_37_38_42", base)
  
  out_name <- base
  
  # Write output
  writeRaster(
    out,
    file.path(out_dir, out_name),
    overwrite = TRUE
  )
  
  cat("Finished:", out_name, "\n")
  
  rm(r37, r38, r42,
     r37_m, r38_m, r42_m,
     out)
  gc()
}


