library(terra)
library(data.table)
library(dplyr)
library(ggplot2)
library(flextable)   # for Word tables
library(officer)     # for exporting to Word


#########################################################################
##############################JULY WEATHER##############################
#########################################################################

##Load df
df <- fread("Z:/Christine/NWT Fire Refugia Project Z/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")

df <- as.data.frame(df)
df$ecozone <- as.factor(df$ecozone)

################################################################################
# CREATE CUSTOM FRU GROUPS
################################################################################

df$FRU_GROUP <- NA

# Ecozone split example
df$FRU_GROUP[df$FRU == 26 & df$ecozone %in% c(5, 6)] <- "FRU_26_shield" #Shield

df$FRU_GROUP[df$FRU == 26 & df$ecozone %in% c(4, 9)] <- "FRU_26" #Plains

# Everything else stays as its own FRU
remaining <- is.na(df$FRU_GROUP)
df$FRU_GROUP[remaining] <- paste0("FRU_", df$FRU[remaining])

# List of all FRU groups
all_FRUs <- sort(unique(df$FRU_GROUP))

################################################################################
# FILTER JULY PERIOD
################################################################################


# Summarise for the filtered DOB range
weather_table <- df %>%
  group_by(FRU_GROUP) %>%
  summarise(
    vpd_mean = mean(vpd, na.rm = TRUE),
    isi_mean = mean(isi, na.rm = TRUE),
    bui_mean = mean(bui, na.rm = TRUE),
    rh_mean = mean(rh, na.rm = TRUE),
  ) %>%
  right_join(data.frame(FRU_GROUP = all_FRUs), by = "FRU_GROUP") %>%
  arrange(FRU_GROUP)
weather_table

weather_df <- as.data.frame(weather_table)


vars <- c(
  "vpd_mean",
  "isi_mean",
  "bui_mean",
  "rh_mean")



###Start creating rasters
##Load ROI and make all values 0
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")
ROI <- ROI*0

out_dir <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/For CIF Conference/Weather/"

for (v in vars) {
  
  for (i in 1:nrow(weather_df)) {
    
    fru_id <- weather_df$FRU_GROUP[i]
    
    val <- weather_df[[v]][i]
    
    r <- ROI + val
    
    ## Default folder name
    folder_name <- gsub("_", " ", fru_id)
    
    ## Special cases
    if (fru_id == "FRU_26_shield") {
      folder_name <- "FRU 26 shield"
    }
    
    if (fru_id == "FRU_26") {
      folder_name <- "FRU 26"
    }
    
    ## CREATE FOLDER IF IT DOESN'T EXIST
    dir.create(
      file.path(out_dir, folder_name),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    ## Output file
    out_file <- file.path(
      out_dir,
      folder_name,
      paste0(v, "_Mean.tif")
    )
    
    ## Export
    writeRaster(r, out_file, overwrite = TRUE)
    
    gc()
  }
}



#Now bring the merged ones back in and process them into a single region
######################################################
#####FIRE WEATHER 24/26 
######################################################
#--------------------------------------------------
# Paths
#--------------------------------------------------
weather_base <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/For CIF Conference/Weather/For singles before merging"
weather_out <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/For CIF Conference/Weather/FRU 24 26"

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


######################################################
#####FIRE WEATHER 37/38/42
######################################################
#--------------------------------------------------
# Paths
#--------------------------------------------------
weather_base <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/For CIF Conference/Weather/For singles before merging"
weather_out <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/For CIF Conference/Weather/FRU 37 38 42"

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
