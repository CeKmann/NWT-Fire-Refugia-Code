library(terra)
library(dplyr)
library(tibble)



###########################################################
########################For 24/26##########################
###########################################################

######################################################
#####FIRE WEATHER
######################################################
#--------------------------------------------------
# Paths
#--------------------------------------------------
weather_base <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Fire Weather V6/2023/For singles before merging"
weather_out <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Fire Weather V6/2023/FRU 24 26"

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
weather_base <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Fire Weather V6/2023/For singles before merging"
weather_out <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Fire Weather V6/2023/FRU 37 38 42"

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

