library(terra)

FRUs <- rast("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Full NWT FRUs 120m.tif")

# Values to keep
keep <- c(37, 38, 40, 41, 42, 43, 45, 46)

# Keep only those values and set them to 1
FRUs <- ifel(FRUs %in% keep, 1, NA)
FRUs
plot(FRUs)

writeRaster(FRUs, filename = "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Mountains.tif")



#Find the hot years in the mountains

climate_dir <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Climate Anoms V6"

mask_raster <- rast("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Mountains.tif")

#Read in the MAT anom files
files <- list.files(
  climate_dir,
  pattern = "MAT.*\\.tif$",
  full.names = TRUE
)

#Find the mean of each raster
means <- numeric(length(files))

for(i in seq_along(files)){
  
  r <- rast(files[i])
  
  r <- mask(r, mask_raster) #This step for specific FRUs only. Comment out if using code elsewhere
  
  means[i] <- global(r, "mean", na.rm = TRUE)[1,1]
  
  rm(r)
  gc()
}


results <- data.frame(
  file = basename(files),
  mean = means
)
gc()

results <- results[order(results$mean), ]
results


#Find the hottest years
p90 <- quantile(results$mean, 0.90) #90th percentile is for hot

hot_year <- results[which.min(abs(results$mean - p90)), ]

hot_year #Identifies 2015 for the hot year 

