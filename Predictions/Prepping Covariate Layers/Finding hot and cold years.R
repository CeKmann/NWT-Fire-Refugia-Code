library(terra)

climate_dir <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Climate Anoms V6"

#mask_raster <- rast("Z:/Christine/NWT Fire Refugia Project Z/Predictions/V6/FRUs/FRU_X.tif")

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
  
  #r <- mask(r, mask_raster) #This step for specific FRUs only. Comment out if using code elsewhere
  
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


#From those means, find the 10th and 90th percentile values, then find the year which is closest to that
p10 <- quantile(results$mean, 0.10) #10th percentile is for cold
p90 <- quantile(results$mean, 0.90) #90th percentile is for hot

cold_year <- results[which.min(abs(results$mean - p10)), ]
hot_year <- results[which.min(abs(results$mean - p90)), ]

cold_year #Identifies 2020 for the cold year
hot_year #Identifies 2006 for the hot year 

