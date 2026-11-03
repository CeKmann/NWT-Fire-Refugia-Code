library(terra)

#################################################
#################CLIMATE NORMALS#################
#################################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Climate Normals V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Climate Normals V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Climate Normals V6/", Layers[a]))
  gc()
}

#Do the processed rasters match the ROI one?
compareGeom(ROI, Raster_Mask, stopOnError = FALSE) #Shows that all the rasters match




###############################################
#################CLIMATE ANOMS#################
###############################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Climate Anoms V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Climate Anoms V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Climate Anoms V6/", Layers[a]))
  gc()
}

#Do the processed rasters match the ROI one?
compareGeom(ROI, Raster_Mask, stopOnError = FALSE) #Shows that all the rasters match




#################################################
###################DISTURBANCE###################
#################################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Disturbance V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Disturbance V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Disturbance V6/", Layers[a]))
  gc()
}



#################################################
#######################SOILS#####################
#################################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Soils V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Soils V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Soils V6/", Layers[a]))
  gc()
}


#################################################
#####################TERRAIN#####################
#################################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Terrain V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Terrain V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Terrain V6/", Layers[a]))
  gc()
}




#################################################
#####################PONTONE#####################
#################################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Pontone V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Pontone V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Pontone V6/", Layers[a]))
  gc()
}



#################################################
#####################FUELS#######################
#################################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Fuels V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Fuels V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Fuels V6/", Layers[a]))
  gc()
}


#################################################
#####################OPEN AREAS EXTRAS###########
#################################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Open and Extra V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Open and Extra V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Open and Extra V6/", Layers[a]))
  gc()
}



##########################################
#####################STRAGGLERS###########
##########################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Stragglers V6",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Stragglers V6/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Stragglers V6/", Layers[a]))
  gc()
}



#################################################
#####################Climate Anoms#####################
#################################################

##Load ROI
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

##Read in a list of all the layers that need to be processed:
Layers <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Climate Anoms V6/Anoms 2023",
                     pattern = ".tif$")
Layers

#a = 1
for (a in 1:length(Layers)){
  Raster <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Raw from GEE/Climate Anoms V6/Anoms 2023/", Layers[a], sep = ""))
  Raster_Crop <- crop(Raster, ROI)
  Raster_Mask <- mask(Raster_Crop, ROI)
  
  writeRaster(Raster_Mask, filename = paste0("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Climate Anoms V6/", Layers[a]))
  gc()
}
