library(terra)

ROI <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")
ROI <- ROI*0

Date <- 200 #July 19

DOB <- ROI + Date

writeRaster(DOB, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/DOB V6/DOB.tif")
