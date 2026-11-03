library(terra)
library(dplyr)
library(data.table)

#Load the sample and the model
df <- fread("Z:/Christine/NWT Fire Refugia Project Z/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)
  df$FRU <- as.factor(df$FRU)
  df$ecozone <- as.factor(df$ecozone)

#Load the RSF (already projected and resampled to 120m)
RSF <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Caribou/RSF/2020/RSF_2020_Calving_120m.tif")


#Convert the points to a spatial object
points_vect <- vect(df, geom = c("Lam_x", "Lam_y"), crs = crs(RSF))

# Extract raster values at point locations
values <- terra::extract(RSF, points_vect)

# Find indices where the raster value is NOT NA
valid_idx <- !is.na(values[, 2])  # assuming 2nd column holds raster values

# Subset original dataframe for points overlapping non-NA raster pixels
df_valid <- df[valid_idx, ]


############A double check to make sure all retained points do overlap non-NA RSF pixels######
  # Convert filtered points to SpatVector
  valid_points_vect <- vect(df_valid, geom = c("Lam_x", "Lam_y"), crs = crs(RSF))
  
  # Extract raster values at filtered points
  valid_values <- terra::extract(RSF, valid_points_vect)
  
  # Check if any extracted values are NA
  any_na <- any(is.na(valid_values[, 2]))
  
  if(any_na) {
    message("Warning: Some points in df_valid still overlap NA raster pixels.")
  } else {
    message("All points in df_valid overlap non-NA raster pixels.")
  } #Says it's all good. Came from ChatGPT
########################################################################################

#Now find the optimal threshold value for points found within the FULL range
#To do this, Diana suggests finding the % refugia for this area and using that
mean(df_valid$Refugia) #0.1552978
