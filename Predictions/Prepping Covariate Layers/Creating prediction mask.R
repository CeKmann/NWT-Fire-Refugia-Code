library(terra)

ROI <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

mask_30m <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Sampling Mask/NALCMS_2020_Sampling_Mask.tif")

Mask_120m <- resample(mask_30m, ROI, method = "near")
compareGeom(Mask_120m, ROI) #True
Mask_120m

writeRaster(Mask_120m, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Sampling Mask/Pred_Mask_120m.tif")
