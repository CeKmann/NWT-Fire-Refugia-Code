library(terra)

#Load masked Topography predictions and RSF
Topo_Preds <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Predictions/Mosaics/Masked/Masked_NWT_Topo_Median_Mosaic_V6.tif")
RSF <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Original Data/Caribou RSF/Later versions/2020/Rasters/Summer_2020.tif")

#This is for the projected RSF rasters:
#For whatever reason, the no-data value assigned when exporting from Arc will mess with the RSF at the projection step. This code will fix that
# NAflag(RSF) <- 0
# RSF[RSF == 0] <- NA
# gc()

#Project and resample the RSF to match the Topography preds. Because values are categorical, use nearest neighbour
RSF_Resamp <- project(RSF, Topo_Preds, res = 120, method = "near")
  RSF_Resamp
  #plot(RSF_Resamp)

#I added this step to write the unmasked versions to disc at a Later point (previously had just gone straight through to masking), so don't worry about file paths here  
writeRaster(RSF_Resamp, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Processing Data/Caribou/RSF/2020/RSF_2020_Summer_120m_Unmasked.tif")
gc()


#Load the mask and use it on the resampled RSF
Mask <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Sampling Mask/Pred_Mask_120m.tif")
RSF_Masked <- mask(RSF_Resamp, Mask, maskvalue=1, updatevalue=NA)
gc()
  RSF_Masked
  plot(RSF_Masked)

writeRaster(RSF_Masked, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Caribou/RSF/2020/RSF_2020_Summer_120m.tif")
gc()
