library(terra)

ROI <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

Clay <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Not Filled/Clay.tif")
Clay_Filled <- subst(Clay, NA, 0) #Fills in the bounding box too
Clay_Mask <- mask(Clay_Filled, ROI) #This gets rid of that
writeRaster(Clay_Mask, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Clay.tif")

Coarse <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Not Filled/Coarse.tif")
Coarse_Filled <- subst(Coarse, NA, 0) #Fills in the bounding box too
Coarse_Mask <- mask(Coarse_Filled, ROI) #This gets rid of that
writeRaster(Coarse_Mask, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Coarse.tif")

Sand <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Not Filled/Sand.tif")
Sand_Filled <- subst(Sand, NA, 0) #Fills in the bounding box too
Sand_Mask <- mask(Sand_Filled, ROI) #This gets rid of that
writeRaster(Sand_Mask, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Sand.tif")

Silt <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Not Filled/Silt.tif")
Silt_Filled <- subst(Silt, NA, 0) #Fills in the bounding box too
Silt_Mask <- mask(Silt_Filled, ROI) #This gets rid of that
writeRaster(Silt_Mask, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Silt.tif")

Permafrost <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Not Filled/Permafrost.tif")
Permafrost_Filled <- subst(Permafrost, NA, 0) #Fills in the bounding box too
Permafrost_Mask <- mask(Permafrost_Filled, ROI) #This gets rid of that
writeRaster(Permafrost_Mask, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Soils V6/Permafrost.tif")
