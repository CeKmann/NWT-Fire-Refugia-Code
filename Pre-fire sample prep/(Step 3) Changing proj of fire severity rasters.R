library(terra)

ROI <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_30m.tif")

fire_files <- list.files(
  "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Original Data/Fires/Rasters",
  pattern="\\.tif$",
  full.names=TRUE
)

out_dir <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Fires/Rasters"
dir.create(out_dir, showWarnings = FALSE)

for (i in seq_along(fire_files)) {
  
  r <- rast(fire_files[i])
  r_proj <- project(r, ROI, method="near")
  
  outfile <- file.path(out_dir, paste0("Final_", basename(fire_files[i])))
  writeRaster(r_proj, outfile, overwrite=TRUE, gdal="COMPRESS=LZW") #Compress so files aren't so huge
  
  gc()
} #Just remember that, for whatever reason, these rasters will load into Arc as numbers (wasn't the case for the AB ones). However, the values still correspond to the original and they still load properly in R. Chat says this is still fine for extraction in R



#When it's done, test it on one layer to make sure everything aligns with the base
ROI <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_30m.tif")
Out <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Fires/Rasters/Final_BSC_2002_NWT_Base_Lam_v20250527.tif")
terra::compareGeom(ROI, Out)
